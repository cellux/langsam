package langsam

import (
	"bufio"
	_ "embed"
	"fmt"
	"io"
	"maps"
	"math"
	"strconv"
	"strings"
	"sync"
)

//go:embed langsam.l
var langsamL string

// value types

type Value any

type Error struct {
	Payload Value
}

type Boolean bool
type Integer int64
type Float float64
type String string

type Symbol struct {
	Name string
}

type Keyword struct {
	Sym *Symbol
}

type Cons struct {
	Car Value
	Cdr Value
}

type List []Value
type Vector []Value
type Map map[Value]Value

type Function struct {
	Name       *Symbol
	ParamNames []*Symbol
	RestName   *Symbol
	EvalArgs   bool
	EvalResult bool
	Body       Value
}

type NativeFn func(vm *VM, args []Value) Value

// value interfaces

type IType interface {
	Type(vm *VM) Value
}

type IEq interface {
	Eq(vm *VM, rhs Value) bool
}

type ICmp interface {
	Cmp(vm *VM, rhs Value) Value
}

type IAdd interface {
	Add(vm *VM, rhs Value) Value
}

type ISub interface {
	Sub(vm *VM, rhs Value) Value
}

type IMul interface {
	Mul(vm *VM, rhs Value) Value
}

type IDiv interface {
	Div(vm *VM, rhs Value) Value
}

type ICall interface {
	Call(vm *VM, args []Value) Value
}

type ILen interface {
	Len(vm *VM) int
}

type IDeref interface {
	Deref(vm *VM) Value
}

type IEval interface {
	Eval(vm *VM) Value
}

type IRepr interface {
	Repr() string
}

// Value

func Str(v Value) string {
	if v == nil {
		return "nil"
	}
	if i, ok := v.(fmt.Stringer); ok {
		return i.String()
	} else {
		return fmt.Sprintf("%v", v)
	}
}

func Repr(v Value) string {
	if v == nil {
		return "nil"
	}
	if i, ok := v.(IRepr); ok {
		return i.Repr()
	} else {
		return Str(v)
	}
}

// Nil

var symNilType = internSymbol("Nil")

func NilType(vm *VM, args []Value) Value {
	return nil
}

// Error

var symErrorType = internSymbol("Error")

func ErrorType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("Error expects a single argument, got %d", len(args))
	}
	arg := args[0]
	switch arg.(type) {
	case Error:
		return arg
	default:
		return Error{arg}
	}
}

func IsError(v Value) bool {
	_, ok := v.(Error)
	return ok
}

func IsRuntimeException(v Value) bool {
	if e, ok := v.(Error); ok {
		if c, ok := e.Payload.(*Cons); ok {
			if sym, ok := c.Car.(*Symbol); ok {
				if len(sym.Name) > 0 && sym.Name[0] == '!' {
					return true
				}
			}
		}
	}
	return false
}

func Errorf(format string, args ...any) Value {
	err := fmt.Sprintf(format, args...)
	return Error{
		Payload: String(err),
	}
}

func evalError(vm *VM, args []Value) Value {
	if len(args) == 0 {
		return vm.RuntimeExceptionf("error expects at least one argument")
	}
	arg := args[0]
	switch v := arg.(type) {
	case String:
		templateArgs := make([]any, len(args)-1)
		for i := range len(args) - 1 {
			templateArgs[i] = args[i+1]
		}
		return Errorf(string(v), templateArgs...)
	default:
		if len(args) > 1 {
			return vm.RuntimeExceptionf("error called with value of type %T followed by extra arguments", arg)
		}
		return Error{arg}
	}
}

func (e Error) Type(vm *VM) Value {
	return vm.rootlet[symErrorType]
}

func (e Error) Deref(vm *VM) Value {
	return e.Payload
}

func (e Error) String() string {
	if c, ok := e.Payload.(*Cons); ok {
		return Str(c.Cdr)
	} else {
		return Str(e.Payload)
	}
}

func (e Error) Repr() string {
	return fmt.Sprintf("(error '%s)", Repr(e.Payload))
}

// Boolean

var symBooleanType = internSymbol("Boolean")

func BooleanType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("Boolean expects one argument, got %d", len(args))
	}
	arg := args[0]
	if arg == nil {
		return FalseValue
	}
	if coll, ok := arg.(ILen); ok {
		if coll.Len(vm) == 0 {
			return FalseValue
		} else {
			return TrueValue
		}
	}
	switch v := arg.(type) {
	case Boolean:
		return arg
	case Integer:
		if v == 0 {
			return FalseValue
		} else {
			return TrueValue
		}
	case Float:
		if v == 0.0 {
			return FalseValue
		} else {
			return TrueValue
		}
	case *Symbol, Keyword, *Function:
		return TrueValue
	case *Cons:
		if v == nil {
			return FalseValue
		} else {
			return TrueValue
		}
	default:
		return vm.RuntimeExceptionf("cannot cast value of type %T to Boolean", arg)
	}
}

var TrueValue = Boolean(true)
var FalseValue = Boolean(false)

func (b Boolean) Type(vm *VM) Value {
	return vm.rootlet[symBooleanType]
}

func (b Boolean) Eval(vm *VM) Value {
	return b
}

func (b1 Boolean) Eq(vm *VM, v2 Value) bool {
	if b2, ok := v2.(Boolean); ok {
		return b1 == b2
	} else {
		return false
	}
}

func (b Boolean) String() string {
	if b == TrueValue {
		return "true"
	} else {
		return "false"
	}
}

// numbers

func readIntegerInRadix(src io.ByteScanner, radix int64) (int64, error) {
	var value int64 = 0
	for {
		b, err := src.ReadByte()
		if err != nil {
			return 0, err
		}
		if b >= 'a' {
			b = b - ('a' - 'A')
		}
		if b >= 'A' {
			b = b - ('A' - '9') + 1
		}
		digit := int64(b) - '0'
		if digit < 0 || digit >= radix {
			err := src.UnreadByte()
			if err != nil {
				return 0, err
			}
			return value, nil
		}
		value = (value * radix) + digit
	}
}

func readFloat(src io.ByteScanner, integerPart int64) (float64, error) {
	var value float64 = float64(integerPart)
	var divisor float64 = 1
	for {
		b, err := src.ReadByte()
		if err != nil {
			return 0, err
		}
		if b < '0' || b > '9' {
			err := src.UnreadByte()
			if err != nil {
				return 0, err
			}
			return value / divisor, nil
		}
		digit := float64(b) - '0'
		value = (value * 10) + digit
		divisor *= 10
	}
}

func readNumber(src io.ByteScanner, b byte) (Value, error) {
	value := int64(b) - '0'
	checkPrefix := true
	for {
		b, err := src.ReadByte()
		if err != nil {
			return nil, err
		}
		if checkPrefix && value == 0 {
			switch b {
			case 'b':
				return readIntegerInRadix(src, 2)
			case 'o':
				return readIntegerInRadix(src, 8)
			case 'x':
				return readIntegerInRadix(src, 16)
			}
		}
		if b == '.' {
			f, err := readFloat(src, value)
			if err != nil {
				return nil, err
			}
			return Float(f), nil
		}
		if b < '0' || b > '9' {
			err := src.UnreadByte()
			if err != nil {
				return nil, err
			}
			return Integer(value), nil
		}
		digit := int64(b) - '0'
		value = (value * 10) + digit
		checkPrefix = false
	}
}

// Integer

var symIntegerType = internSymbol("Integer")

func IntegerType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("Integer expects one argument, got %d", len(args))
	}
	arg := args[0]
	switch arg.(type) {
	case Integer:
		return arg
	case Float:
		return AsInteger(vm, arg)
	default:
		return vm.RuntimeExceptionf("cannot cast value of type %T to Integer", arg)
	}
}

func (i Integer) Type(vm *VM) Value {
	return vm.rootlet[symIntegerType]
}

func (i1 Integer) Cmp(vm *VM, v2 Value) Value {
	switch v := v2.(type) {
	case Integer:
		lhs := i1
		rhs := v
		if lhs == rhs {
			return Integer(0)
		} else if lhs < rhs {
			return Integer(-1)
		} else {
			return Integer(1)
		}
	case Float:
		lhs := Float(i1)
		rhs := v
		if lhs == rhs {
			return Integer(0)
		} else if lhs < rhs {
			return Integer(-1)
		} else {
			return Integer(1)
		}
	default:
		return vm.RuntimeExceptionf("cannot compare Integer with value of type %T", v2)
	}
}

func (i Integer) Eval(vm *VM) Value {
	return i
}

func AsInteger(vm *VM, arg Value) Value {
	switch v := arg.(type) {
	case Integer:
		return v
	case Float:
		return Integer(int64(math.Round(float64(v))))
	default:
		return vm.RuntimeExceptionf("cannot cast value of type %T to Integer", arg)
	}
}

func (i1 Integer) Add(vm *VM, v2 Value) Value {
	i2 := AsInteger(vm, v2)
	if IsError(i2) {
		return i2
	}
	return Integer(i1 + i2.(Integer))
}

func (i1 Integer) Sub(vm *VM, v2 Value) Value {
	i2 := AsInteger(vm, v2)
	if IsError(i2) {
		return i2
	}
	return Integer(i1 - i2.(Integer))
}

func (i1 Integer) Mul(vm *VM, v2 Value) Value {
	i2 := AsInteger(vm, v2)
	if IsError(i2) {
		return i2
	}
	return Integer(i1 * i2.(Integer))
}

func (i1 Integer) Div(vm *VM, v2 Value) Value {
	i2 := AsInteger(vm, v2)
	if IsError(i2) {
		return i2
	}
	if i2.(Integer) == 0 {
		return vm.RuntimeExceptionf("integer division by zero")
	}
	return Integer(i1 / i2.(Integer))
}

func (i Integer) String() string {
	return fmt.Sprintf("%d", i)
}

// Float

var symFloatType = internSymbol("Float")

func FloatType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("Float expects one argument, got %d", len(args))
	}
	arg := args[0]
	switch arg.(type) {
	case Float:
		return arg
	case Integer:
		return AsFloat(vm, arg)
	default:
		return vm.RuntimeExceptionf("cannot cast value of type %T to Float", arg)
	}
}

func (f Float) Type(vm *VM) Value {
	return vm.rootlet[symFloatType]
}

func (f1 Float) Cmp(vm *VM, v2 Value) Value {
	switch v := v2.(type) {
	case Integer:
		lhs := f1
		rhs := Float(v)
		if lhs == rhs {
			return Integer(0)
		} else if lhs < rhs {
			return Integer(-1)
		} else {
			return Integer(1)
		}
	case Float:
		lhs := f1
		rhs := v
		if lhs == rhs {
			return Integer(0)
		} else if lhs < rhs {
			return Integer(-1)
		} else {
			return Integer(1)
		}
	default:
		return vm.RuntimeExceptionf("cannot compare Float with value of type %T", v2)
	}
}

func (f Float) Eval(vm *VM) Value {
	return f
}

func AsFloat(vm *VM, arg Value) Value {
	switch v := arg.(type) {
	case Integer:
		return Float(float64(v))
	case Float:
		return v
	default:
		return vm.RuntimeExceptionf("cannot cast value of type %T to Float", arg)
	}
}

func (f1 Float) Add(vm *VM, v2 Value) Value {
	f2 := AsFloat(vm, v2)
	if IsError(f2) {
		return f2
	}
	return Float(f1 + f2.(Float))
}

func (f1 Float) Sub(vm *VM, v2 Value) Value {
	f2 := AsFloat(vm, v2)
	if IsError(f2) {
		return f2
	}
	return Float(f1 - f2.(Float))
}

func (f1 Float) Mul(vm *VM, v2 Value) Value {
	f2 := AsFloat(vm, v2)
	if IsError(f2) {
		return f2
	}
	return Float(f1 * f2.(Float))
}

func (f1 Float) Div(vm *VM, v2 Value) Value {
	f2 := AsFloat(vm, v2)
	if IsError(f2) {
		return f2
	}
	if f2.(Float) == 0.0 {
		return vm.RuntimeExceptionf("float division by zero")
	}
	return Float(f1 / f2.(Float))
}

func (f Float) String() string {
	s := strconv.FormatFloat(float64(f), 'f', -1, 64)
	if !strings.ContainsRune(s, '.') {
		s += ".0"
	}
	return s
}

// String

var symStringType = internSymbol("String")

func StringType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("String expects a single argument, got %d", len(args))
	}
	arg := args[0]
	switch arg.(type) {
	case String:
		return arg
	default:
		return vm.RuntimeExceptionf("cannot cast value of type %T to String", arg)
	}
}

func (s String) Type(vm *VM) Value {
	return vm.rootlet[symStringType]
}

func (s1 String) Cmp(vm *VM, v2 Value) Value {
	if s2, ok := v2.(String); ok {
		lhs := string(s1)
		rhs := string(s2)
		return Integer(strings.Compare(lhs, rhs))
	} else {
		return vm.RuntimeExceptionf("cannot compare String with value of type %T", v2)
	}
}

func (s String) Len(vm *VM) int {
	return len(s)
}

func readString(src io.ByteScanner) (Value, error) {
	var sb strings.Builder
	for {
		b, err := src.ReadByte()
		if err != nil {
			return nil, err
		}
		if b == '"' {
			return String(sb.String()), nil
		}
		if b == '\\' {
			b, err = src.ReadByte()
			if err != nil {
				return nil, err
			}
			switch b {
			case 'a':
				b = 0x07
			case 'b':
				b = 0x08
			case 'f':
				b = 0x0c
			case 'n':
				b = 0x0a
			case 'r':
				b = 0x0d
			case 't':
				b = 0x09
			case 'v':
				b = 0x0b
			}
		}
		sb.WriteByte(b)
	}
}

func (s String) Eval(vm *VM) Value {
	return s
}

func (s String) String() string {
	return string(s)
}

func (s String) Repr() string {
	return fmt.Sprintf("%#v", string(s))
}

// Symbol

var symSymbolType = internSymbol("Symbol")

func SymbolType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("Symbol expects one argument, got %d", len(args))
	}
	arg := args[0]
	switch v := arg.(type) {
	case *Symbol:
		return arg
	case String:
		return internSymbol(string(v))
	default:
		return vm.RuntimeExceptionf("cannot cast value of type %T to Symbol", arg)
	}
}

func (sym *Symbol) Type(vm *VM) Value {
	return vm.rootlet[symSymbolType]
}

func (sym1 *Symbol) Eq(vm *VM, v2 Value) bool {
	if sym2, ok := v2.(*Symbol); ok {
		return sym1 == sym2
	} else {
		return false
	}
}

var symtab = make(map[string]*Symbol)

func internSymbol(name string) *Symbol {
	sym, ok := symtab[name]
	if !ok {
		sym = &Symbol{name}
		symtab[name] = sym
	}
	return sym
}

func isWhiteSpace(b byte) bool {
	return b <= 0x20
}

func isSymbolChar(b byte) bool {
	if isWhiteSpace(b) {
		return false
	}
	if b == '(' || b == ')' {
		return false
	}
	if b == '[' || b == ']' {
		return false
	}
	if b == '{' || b == '}' {
		return false
	}
	return true
}

func readSymbol(src io.ByteScanner, firstByte byte) (Value, error) {
	var sb strings.Builder
	sb.WriteByte(firstByte)
	for {
		b, err := src.ReadByte()
		if err != nil {
			return nil, err
		}
		if !isSymbolChar(b) {
			err := src.UnreadByte()
			if err != nil {
				return nil, err
			}
			return internSymbol(sb.String()), nil
		}
		sb.WriteByte(b)
	}
}

func (sym *Symbol) Eval(vm *VM) Value {
	return vm.Lookup(sym)
}

func (sym *Symbol) String() string {
	return sym.Name
}

// Keyword

var symKeywordType = internSymbol("Keyword")

func KeywordType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("Keyword expects one argument, got %d", len(args))
	}
	arg := args[0]
	switch v := arg.(type) {
	case Keyword:
		return arg
	case String:
		return internKeyword(string(v))
	case *Symbol:
		return internKeyword(v.Name)
	default:
		return vm.RuntimeExceptionf("cannot cast value of type %T to Keyword", arg)
	}
}

func (kw Keyword) Type(vm *VM) Value {
	return vm.rootlet[symKeywordType]
}

func (kw1 Keyword) Eq(vm *VM, v2 Value) bool {
	if kw2, ok := v2.(Keyword); ok {
		return kw1 == kw2
	} else {
		return false
	}
}

func internKeyword(s string) Keyword {
	sym := internSymbol(s)
	return Keyword{sym}
}

func readKeyword(src io.ByteScanner) (Value, error) {
	var sb strings.Builder
	for {
		b, err := src.ReadByte()
		if err != nil {
			return nil, err
		}
		if !isSymbolChar(b) {
			err := src.UnreadByte()
			if err != nil {
				return nil, err
			}
			return internKeyword(sb.String()), nil
		}
		sb.WriteByte(b)
	}
}

func (kw Keyword) Eval(vm *VM) Value {
	return kw
}

func (kw Keyword) String() string {
	return fmt.Sprintf(":%s", kw.Sym)
}

// Cons

var symConsType = internSymbol("Cons")

func consFromSlice(slice []Value, lastCdr Value) Value {
	if len(slice) == 0 {
		return nil
	}
	result := lastCdr
	for i := len(slice) - 1; i >= 0; i-- {
		result = &Cons{
			Car: slice[i],
			Cdr: result,
		}
	}
	return result
}

func ConsType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("Cons expects a single argument, got %d", len(args))
	}
	arg := args[0]
	switch v := arg.(type) {
	case *Cons:
		return arg
	case List:
		return consFromSlice(v, nil)
	case Vector:
		return consFromSlice(v, nil)
	default:
		return vm.RuntimeExceptionf("cannot cast value of type %T to Cons", arg)
	}
}

func (c *Cons) Type(vm *VM) Value {
	return vm.rootlet[symConsType]
}

func (c1 *Cons) Eq(vm *VM, v2 Value) bool {
	if c2, ok := v2.(*Cons); ok {
		return vm.Eq(c1.Car, c2.Car) && vm.Eq(c1.Cdr, c2.Cdr)
	} else {
		return false
	}
}

func (c *Cons) String() string {
	return c.Repr()
}

func (c *Cons) Repr() string {
	if c == nil {
		return "nil"
	}
	var sb strings.Builder
	sb.WriteByte('(')
	for c != nil {
		sb.WriteString(Repr(c.Car))
		if tail, ok := c.Cdr.(*Cons); ok {
			if tail != nil {
				sb.WriteString(" ")
			}
			c = tail
		} else {
			sb.WriteString(" . ")
			sb.WriteString(Repr(c.Cdr))
			break
		}
	}
	sb.WriteByte(')')
	return sb.String()
}

func evalCons(vm *VM, args []Value) Value {
	if len(args) != 2 {
		return vm.RuntimeExceptionf("cons expects two arguments, got %d", len(args))
	}
	return &Cons{
		Car: args[0],
		Cdr: args[1],
	}
}

func evalCar(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("car expects one argument, got %d", len(args))
	}
	if c, ok := args[0].(*Cons); ok {
		return c.Car
	} else {
		return vm.RuntimeExceptionf("car expects a Cons argument, got %T", args[0])
	}
}

func evalCdr(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("cdr expects one argument, got %d", len(args))
	}
	if c, ok := args[0].(*Cons); ok {
		return c.Cdr
	} else {
		return vm.RuntimeExceptionf("cdr expects a Cons argument, got %T", args[0])
	}
}

// List

var symListType = internSymbol("List")

func ListType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("List expects a single argument, got %d", len(args))
	}
	arg := args[0]
	switch v := arg.(type) {
	case List:
		return v
	case Vector:
		return List(v)
	case *Cons:
		var result List
		for v != nil {
			result = append(result, v.Car)
			if cdr, ok := v.Cdr.(*Cons); ok {
				v = cdr
			} else if v.Cdr == nil {
				break
			} else {
				return Errorf("Cons passed to List constructor is not a proper list: found cdr=%#v", v.Cdr)
			}
		}
		return result
	default:
		return vm.RuntimeExceptionf("cannot cast value of type %T to List", arg)
	}
}

func (l List) Type(vm *VM) Value {
	return vm.rootlet[symListType]
}

func readListOrCons(src io.ByteScanner) (Value, error) {
	var value List
	for {
		b, err := src.ReadByte()
		if err != nil {
			return nil, err
		}
		if isWhiteSpace(b) {
			continue
		}
		if b == ')' {
			length := len(value)
			if length >= 3 && value[length-2] == internSymbol(".") {
				return consFromSlice(value[:length-2], value[length-1]), nil
			}
			return value, nil
		} else {
			err := src.UnreadByte()
			if err != nil {
				return nil, err
			}
		}
		v, err := read(src)
		if err != nil {
			return nil, err
		}
		value = append(value, v)
	}
}

func (l1 List) Eq(vm *VM, v2 Value) bool {
	if l2, ok := v2.(List); ok {
		if len(l1) != len(l2) {
			return false
		}
		for i := range l1 {
			if !vm.Eq(l1[i], l2[i]) {
				return false
			}
		}
		return true
	}
	return false
}

func (l List) Len(vm *VM) int {
	return len(l)
}

func (l List) Eval(vm *VM) Value {
	if len(l) == 0 {
		return vm.RuntimeExceptionf("cannot evaluate empty list")
	}
	v0 := vm.Eval(l[0])
	if IsError(v0) {
		return v0
	}
	if v0 == nil {
		return vm.RuntimeExceptionf("'%v' is nil", l[0])
	}
	if f, ok := v0.(ICall); ok {
		return f.Call(vm, l[1:])
	} else {
		return vm.RuntimeExceptionf("head of list (%v) has type %T which is not callable", l[0], v0)
	}
}

func (l List) String() string {
	return l.Repr()
}

func (l List) Repr() string {
	var sb strings.Builder
	sb.WriteByte('(')
	for i := range len(l) {
		if i > 0 {
			sb.WriteString(" ")
		}
		sb.WriteString(Repr(l[i]))
	}
	sb.WriteByte(')')
	return sb.String()
}

// Vector

var symVectorType = internSymbol("Vector")

func VectorType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("Vector expects a single argument, got %d", len(args))
	}
	arg := args[0]
	switch v := arg.(type) {
	case Vector:
		return v
	case List:
		return Vector(v)
	case *Cons:
		var result Vector
		for v != nil {
			result = append(result, v.Car)
			if cdr, ok := v.Cdr.(*Cons); cdr == nil || ok {
				v = cdr
			} else {
				return vm.RuntimeExceptionf("Cons passed to Vector constructor is not a proper list: found cdr=%v", v.Cdr)
			}
		}
		return result
	default:
		return vm.RuntimeExceptionf("cannot cast value of type %T to Vector", arg)
	}
}

func (v Vector) Type(vm *VM) Value {
	return vm.rootlet[symVectorType]
}

func readVector(src io.ByteScanner) (Value, error) {
	var value Vector
	for {
		b, err := src.ReadByte()
		if err != nil {
			return nil, err
		}
		if isWhiteSpace(b) {
			continue
		}
		if b == ']' {
			return value, nil
		} else {
			err := src.UnreadByte()
			if err != nil {
				return nil, err
			}
		}
		v, err := read(src)
		if err != nil {
			return nil, err
		}
		value = append(value, v)
	}
}

func (vec1 Vector) Eq(vm *VM, v2 Value) bool {
	if vec2, ok := v2.(Vector); ok {
		if len(vec1) != len(vec2) {
			return false
		}
		for i := range vec1 {
			if !vm.Eq(vec1[i], vec2[i]) {
				return false
			}
		}
		return true
	}
	return false
}

func (v Vector) Len(vm *VM) int {
	return len(v)
}

func (v Vector) Eval(vm *VM) Value {
	ev := make(Vector, len(v))
	for i := range len(v) {
		if ev[i] = vm.Eval(v[i]); IsRuntimeException(ev[i]) {
			return ev[i]
		}
	}
	return ev
}

func (v Vector) String() string {
	return v.Repr()
}

func (v Vector) Repr() string {
	var sb strings.Builder
	sb.WriteByte('[')
	for i := range len(v) {
		if i > 0 {
			sb.WriteString(" ")
		}
		sb.WriteString(Repr(v[i]))
	}
	sb.WriteByte(']')
	return sb.String()
}

// Map

var symMapType = internSymbol("Map")

func mapFromSlice(vm *VM, slice []Value) Value {
	if len(slice)%2 != 0 {
		return vm.RuntimeExceptionf("slice used as map source must have even number of arguments")
	}
	result := make(Map, len(slice)/2)
	i := 0
	for i < len(slice) {
		k := slice[i]
		i++
		v := slice[i]
		i++
		result[k] = v
	}
	return result
}

func MapType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("Map expects a single argument, got %d", len(args))
	}
	arg := args[0]
	switch v := arg.(type) {
	case Map:
		return v
	case List:
		return mapFromSlice(vm, v)
	case Vector:
		return mapFromSlice(vm, v)
	default:
		return vm.RuntimeExceptionf("cannot cast value of type %T to Map", arg)
	}
}

func (m Map) Type(vm *VM) Value {
	return vm.rootlet[symMapType]
}

func readMap(src io.ByteScanner) (Value, error) {
	m := make(Map)
	for {
		b, err := src.ReadByte()
		if err != nil {
			return nil, err
		}
		if isWhiteSpace(b) {
			continue
		}
		if b == '}' {
			return m, nil
		} else {
			err := src.UnreadByte()
			if err != nil {
				return nil, err
			}
		}
		k, err := read(src)
		if err != nil {
			return nil, err
		}
		v, err := read(src)
		if err != nil {
			return nil, err
		}
		m[k] = v
	}
}

func (m Map) Len(vm *VM) int {
	return len(m)
}

func (m Map) Eval(vm *VM) Value {
	em := make(Map, len(m))
	for k, v := range m {
		var ek, ev Value
		if ek = vm.Eval(k); IsRuntimeException(ek) {
			return ek
		}
		if ev = vm.Eval(v); IsRuntimeException(ev) {
			return ev
		}
		em[ek] = ev
	}
	return em
}

var symProto = internSymbol("%proto")

func (m Map) Lookup(key Value) Value {
	for m != nil {
		if val, found := m[key]; found {
			return val
		}
		if proto, found := m[symProto].(Map); found {
			m = proto
		} else {
			break
		}
	}
	return nil
}

func (m Map) String() string {
	return m.Repr()
}

func (m Map) Repr() string {
	var sb strings.Builder
	sb.WriteByte('{')
	i := 0
	for k, v := range m {
		if i > 0 {
			sb.WriteString(" ")
		}
		sb.WriteString(Repr(k))
		sb.WriteString(" ")
		sb.WriteString(Repr(v))
		i++
	}
	sb.WriteByte('}')
	return sb.String()
}

// Function

var symFunctionType = internSymbol("Function")

func FunctionType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("Function expects one argument, got %d", len(args))
	}
	arg := args[0]
	switch v := arg.(type) {
	case *Function:
		return arg
	case Map:
		m := v
		f := &Function{
			EvalArgs:   true,
			EvalResult: false,
		}
		if v, ok := m[internKeyword("name")]; ok {
			if name, ok := v.(*Symbol); ok {
				f.Name = name
			} else {
				return vm.RuntimeExceptionf("expected symbol at :name, got %T", v)
			}
		}
		if v, ok := m[internKeyword("param-names")]; ok {
			if paramNameVector, ok := v.(Vector); ok {
				paramNames := make([]*Symbol, len(paramNameVector))
				for i := range len(paramNames) {
					if paramName, ok := paramNameVector[i].(*Symbol); ok {
						paramNames[i] = paramName
					} else {
						return vm.RuntimeExceptionf("parameter names must be symbols, got %v", paramNameVector[i])
					}
				}
				f.ParamNames = paramNames
			} else {
				return vm.RuntimeExceptionf("expected vector of symbols at :param-names, got %T", v)
			}
		}
		if v, ok := m[internKeyword("rest-name")]; ok {
			if restName, ok := v.(*Symbol); ok {
				f.RestName = restName
			} else {
				return vm.RuntimeExceptionf("expected symbol at :rest-name, got %T", v)
			}
		}
		if v, ok := m[internKeyword("eval-args?")]; ok {
			if evalArgs, ok := v.(Boolean); ok {
				f.EvalArgs = bool(evalArgs)
			} else {
				return vm.RuntimeExceptionf("expected Boolean at :eval-args?, got %T", v)
			}
		}
		if v, ok := m[internKeyword("eval-result?")]; ok {
			if evalResult, ok := v.(Boolean); ok {
				f.EvalResult = bool(evalResult)
			} else {
				return vm.RuntimeExceptionf("expected Boolean at :eval-result?, got %T", v)
			}
		}
		if v, ok := m[internKeyword("body")]; ok {
			switch body := v.(type) {
			case NativeFn:
				f.Body = body
			case List:
				f.Body = body
			case Vector:
				f.Body = List(body)
			default:
				return vm.RuntimeExceptionf("expected list or native fn at :body, got %T", v)
			}
		}
		return f
	default:
		return vm.RuntimeExceptionf("cannot cast value of type %T to Function", args[0])
	}
}

func (b *Function) Type(vm *VM) Value {
	return vm.rootlet[symFunctionType]
}

func (f *Function) Eval(vm *VM) Value {
	return f
}

func (f *Function) WithParamNames(names ...string) *Function {
	syms := make([]*Symbol, len(names))
	for i := range len(names) {
		syms[i] = internSymbol(names[i])
	}
	f.ParamNames = syms
	return f
}

func (f *Function) WithRestName(name string) *Function {
	f.RestName = internSymbol(name)
	return f
}

func (f *Function) WithEvalArgs(evalArgs bool) *Function {
	f.EvalArgs = evalArgs
	return f
}

func (f *Function) WithEvalResult(evalResult bool) *Function {
	f.EvalResult = evalResult
	return f
}

func (f *Function) Call(vm *VM, args []Value) (result Value) {
	var realArgs []Value
	if f.EvalArgs {
		realArgs = make([]Value, len(args))
		for i := range len(args) {
			realArgs[i] = vm.Eval(args[i])
			if IsRuntimeException(realArgs[i]) {
				return vm.RuntimeExceptionf("%v failed to evaluate arg #%d: %v", f, i, realArgs[i])
			}
		}
	} else {
		realArgs = args
	}
	switch body := f.Body.(type) {
	case List:
		nPosArgs := min(len(realArgs), len(f.ParamNames))
		nBindings := nPosArgs
		if f.RestName != nil {
			nBindings++
		}
		fenv := vm.Sublet(nBindings)
		i := 0
		for i < nPosArgs {
			paramName := f.ParamNames[i]
			fenv[paramName] = realArgs[i]
			i++
		}
		for i < len(f.ParamNames) {
			paramName := f.ParamNames[i]
			fenv[paramName] = nil
			i++
		}
		if f.RestName != nil {
			if i < len(realArgs) {
				fenv[f.RestName] = Vector(realArgs[i:])
			} else {
				fenv[f.RestName] = nil
			}
		}
		result = vm.Inlet(fenv, func() (result Value) {
			for _, form := range body {
				result = vm.Eval(form)
				if IsRuntimeException(result) {
					break
				}
			}
			return result
		})
	case NativeFn:
		result = body(vm, realArgs)
	default:
		return vm.RuntimeExceptionf("invalid function body, expected List or NativeFn, got %T", f.Body)
	}
	if IsError(result) {
		return result
	}
	if f.EvalResult {
		return vm.Eval(result)
	} else {
		return result
	}
}

func (f1 *Function) Eq(vm *VM, v2 Value) bool {
	if f2, ok := v2.(*Function); ok {
		return f1 == f2
	} else {
		return false
	}
}

func (f *Function) String() string {
	return Str(f.Name)
}

var symAmpersand = internSymbol("&")

func evalFn(vm *VM, args []Value) Value {
	options := make(Map)
	var paramForms Vector
	for len(args) > 0 && paramForms == nil {
		switch v := args[0].(type) {
		case *Symbol:
			options[internKeyword("name")] = v
		case Map:
			m := vm.Eval(v)
			if IsError(m) {
				return m
			}
			maps.Copy(options, m.(Map))
		case Vector:
			paramForms = v
		}
		args = args[1:]
	}
	var paramNames []Value
	var restName Value
	restMarker := symAmpersand // &
	seenRestMarker := false
	for i, paramForm := range paramForms {
		if paramSym, ok := paramForm.(*Symbol); ok {
			if seenRestMarker {
				if i != len(paramForms)-1 {
					return vm.RuntimeExceptionf("there must be a single symbol after & in parameter list")
				}
				restName = paramSym
			} else if paramSym == restMarker {
				seenRestMarker = true
			} else {
				paramNames = append(paramNames, paramSym)
			}
		} else {
			return vm.RuntimeExceptionf("parameters must be symbols")
		}
	}
	if paramNames != nil {
		options[internKeyword("param-names")] = Vector(paramNames)
	}
	if restName != nil {
		options[internKeyword("rest-name")] = restName
	}
	if len(args) > 0 {
		options[internKeyword("body")] = List(args)
	}
	return FunctionType(vm, []Value{options})
}

func evalMacro(vm *VM, args []Value) Value {
	result := evalFn(vm, args)
	if IsError(result) {
		return result
	}
	f := result.(*Function)
	f.EvalArgs = false
	f.EvalResult = true
	return f
}

// VM

var symDeref = internSymbol("deref")

var symQuote = internSymbol("quote")
var symQuasiQuote = internSymbol("quasiquote")

var symUnquote = internSymbol("unquote")
var symUnquoteSplicing = internSymbol("unquote-splicing")

func read(src io.ByteScanner) (Value, error) {
	for {
		b, err := src.ReadByte()
		if err != nil {
			return nil, err
		}
		switch {
		case isWhiteSpace(b):
			continue
		case b == ';':
			for {
				b, err := src.ReadByte()
				if err != nil {
					return nil, err
				}
				if b == '\n' {
					break
				}
			}
			continue
		case b == '-':
			first, err := src.ReadByte()
			if err != nil {
				return nil, err
			}
			if first >= '0' && first <= '9' {
				value, err := readNumber(src, first)
				if err != nil {
					return nil, err
				}
				switch v := value.(type) {
				case Integer:
					return -1 * v, nil
				case Float:
					return -1 * v, nil
				default:
					return nil, fmt.Errorf("readNumber returned a non-numeric value: %v", value)
				}
			} else {
				err := src.UnreadByte()
				if err != nil {
					return nil, err
				}
				return readSymbol(src, '-')
			}
		case b >= '0' && b <= '9':
			return readNumber(src, b)
		case b == '"':
			return readString(src)
		case b == '(':
			return readListOrCons(src)
		case b == '[':
			return readVector(src)
		case b == '{':
			return readMap(src)
		case b == ':':
			return readKeyword(src)
		case b == '@':
			form, err := read(src)
			if err != nil {
				return nil, err
			}
			return List([]Value{symDeref, form}), nil
		case b == '\'':
			form, err := read(src)
			if err != nil {
				return nil, err
			}
			return List([]Value{symQuote, form}), nil
		case b == '`':
			form, err := read(src)
			if err != nil {
				return nil, err
			}
			return List([]Value{symQuasiQuote, form}), nil
		case b == ',':
			b, err := src.ReadByte()
			if err != nil {
				return nil, err
			}
			if b == '@' {
				form, err := read(src)
				if err != nil {
					return nil, err
				}
				return List([]Value{symUnquoteSplicing, form}), nil
			} else {
				err := src.UnreadByte()
				if err != nil {
					return nil, err
				}
				form, err := read(src)
				if err != nil {
					return nil, err
				}
				return List([]Value{symUnquote, form}), nil
			}
		default:
			return readSymbol(src, b)
		}
	}
}

type VM struct {
	once    sync.Once
	rootlet Map
	curlet  Map
}

func (vm *VM) RuntimeExceptionf(format string, args ...any) Value {
	return Error{
		Payload: &Cons{
			Car: internSymbol("!RuntimeException"),
			Cdr: Errorf(format, args...),
		},
	}
}

func (vm *VM) Type(v Value) Value {
	if v == nil {
		return vm.rootlet[symNilType]
	}
	if i, ok := v.(IType); ok {
		return i.Type(vm)
	} else {
		return vm.RuntimeExceptionf("type of %v cannot be determined", v)
	}
}

func (vm *VM) Len(v Value) Value {
	if v == nil {
		return Integer(0)
	}
	if i, ok := v.(ILen); ok {
		return Integer(i.Len(vm))
	} else {
		return vm.RuntimeExceptionf("value of type %T does not provide length", v)
	}
}

func (vm *VM) Deref(v Value) Value {
	if i, ok := v.(IDeref); ok {
		return i.Deref(vm)
	} else {
		return vm.RuntimeExceptionf("value of type %T does not support dereference", v)
	}
}

func (vm *VM) Cmp(v1 Value, v2 Value) Value {
	if lhs, ok := v1.(ICmp); ok {
		return lhs.Cmp(vm, v2)
	} else {
		return vm.RuntimeExceptionf("value of type %T does not support comparison", v1)
	}
}

func (vm *VM) Eq(v1 Value, v2 Value) bool {
	if v1 == nil {
		return v2 == nil
	}
	if v2 == nil {
		return v1 == nil
	}
	if lhs, ok := v1.(IEq); ok {
		return lhs.Eq(vm, v2)
	} else if lhs, ok := v1.(ICmp); ok {
		return lhs.Cmp(vm, v2) == Integer(0)
	}
	return false
}

func (vm *VM) Eval(v Value) Value {
	if v == nil {
		return nil
	}
	if i, ok := v.(IEval); ok {
		return i.Eval(vm)
	} else {
		return vm.RuntimeExceptionf("value of type %T does not support evaluation", v)
	}
}

func (vm *VM) Lookup(key Value) Value {
	return vm.curlet.Lookup(key)
}

func (vm *VM) Load(rdr io.Reader) (result Value) {
	src := bufio.NewReader(rdr)
	for {
		form, err := read(src)
		if err != nil {
			if err == io.EOF {
				break
			}
			return vm.RuntimeExceptionf("read failed: %v", err)
		}
		result = vm.Eval(form)
		if IsRuntimeException(result) {
			return vm.RuntimeExceptionf("%v\nevaluation failed: %v", Repr(form), result)
		}
	}
	return result
}

func (vm *VM) LoadString(s string) Value {
	return vm.Load(strings.NewReader(s))
}

func (vm *VM) defineValue(name string, value Value) {
	sym := internSymbol(name)
	vm.curlet[sym] = value
}

func (vm *VM) defineNativeFn(name string, fn NativeFn) *Function {
	sym := internSymbol(name)
	f := &Function{
		Name:       sym,
		EvalArgs:   true,
		EvalResult: false,
		Body:       fn,
	}
	vm.curlet[sym] = f
	return f
}

func (vm *VM) defineSpecialForm(name string, fn NativeFn) *Function {
	return vm.defineNativeFn(name, fn).WithEvalArgs(false).WithEvalResult(false)
}

func (vm *VM) Inlet(let Map, f func() Value) Value {
	oldlet := vm.curlet
	vm.curlet = let
	result := f()
	vm.curlet = oldlet
	return result
}

func (vm *VM) Sublet(size int) Map {
	var let Map
	if size == 0 {
		let = make(Map)
	} else {
		let = make(Map, size+1)
	}
	let[symProto] = vm.curlet
	return let
}

type ImportFn func(vm *VM) error

var registeredModules = make(map[string]ImportFn)

func RegisterModule(name string, importFn ImportFn) {
	registeredModules[name] = importFn
}

func (vm *VM) LoadRegisteredModules() (err error) {
	oldlet := vm.curlet
	vm.curlet = vm.rootlet
	for name, importModule := range registeredModules {
		if err = importModule(vm); err != nil {
			err = fmt.Errorf("import failed for module %s: %v", name, err)
			break
		}
	}
	vm.curlet = oldlet
	return err
}

func evalType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("type expects one argument, got %d", len(args))
	}
	return vm.Type(args[0])
}

func evalLen(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("len expects one argument, got %d", len(args))
	}
	return vm.Len(args[0])
}

func evalDeref(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("deref expects one argument, got %d", len(args))
	}
	return vm.Deref(args[0])
}

func evalStr(vm *VM, args []Value) Value {
	var sb strings.Builder
	for _, v := range args {
		sb.WriteString(Str(v))
	}
	return String(sb.String())
}

func evalRepr(vm *VM, args []Value) Value {
	var sb strings.Builder
	for i, v := range args {
		if i > 0 {
			sb.WriteString(" ")
		}
		sb.WriteString(Repr(v))
	}
	return String(sb.String())
}

func evalEval(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("eval expects one argument, got %d", len(args))
	}
	return vm.Eval(args[0])
}

func evalEq(vm *VM, args []Value) Value {
	if len(args) == 0 {
		return vm.RuntimeExceptionf("= expects at least one argument")
	}
	if len(args) == 1 {
		return TrueValue
	}
	eq := vm.Eq(args[0], args[1])
	if !eq {
		return FalseValue
	}
	if len(args) > 2 {
		return evalEq(vm, args[1:])
	} else {
		return TrueValue
	}
}

func evalNotEq(vm *VM, args []Value) Value {
	eq := evalEq(vm, args)
	if eq == TrueValue {
		return FalseValue
	} else {
		return TrueValue
	}
}

func evalAdd(vm *VM, args []Value) Value {
	if len(args) == 0 {
		return vm.RuntimeExceptionf("+ expects at least one argument")
	}
	if len(args) == 1 {
		return args[0]
	}
	if lhs, ok := args[0].(IAdd); ok {
		rhs := args[1]
		result := lhs.Add(vm, rhs)
		if len(args) > 2 {
			args[1] = result
			return evalAdd(vm, args[1:])
		} else {
			return result
		}
	} else {
		return vm.RuntimeExceptionf("value of type %T does not support +", args[0])
	}
}

func evalSub(vm *VM, args []Value) Value {
	if len(args) == 0 {
		return vm.RuntimeExceptionf("- expects at least one argument")
	}
	if len(args) == 1 {
		return evalMul(vm, []Value{args[0], Integer(-1)})
	}
	if lhs, ok := args[0].(ISub); ok {
		rhs := args[1]
		result := lhs.Sub(vm, rhs)
		if len(args) > 2 {
			args[1] = result
			return evalSub(vm, args[1:])
		} else {
			return result
		}
	} else {
		return vm.RuntimeExceptionf("value of type %T does not support -", args[0])
	}
}

func evalMul(vm *VM, args []Value) Value {
	if len(args) == 0 {
		return vm.RuntimeExceptionf("* expects at least one argument")
	}
	if len(args) == 1 {
		return args[0]
	}
	if lhs, ok := args[0].(IMul); ok {
		rhs := args[1]
		result := lhs.Mul(vm, rhs)
		if len(args) > 2 {
			args[1] = result
			return evalMul(vm, args[1:])
		} else {
			return result
		}
	} else {
		return vm.RuntimeExceptionf("value of type %T does not support *", args[0])
	}
}

func evalDiv(vm *VM, args []Value) Value {
	if len(args) == 0 {
		return vm.RuntimeExceptionf("/ expects at least one argument")
	}
	if len(args) == 1 {
		return args[0]
	}
	if lhs, ok := args[0].(IDiv); ok {
		rhs := args[1]
		result := lhs.Div(vm, rhs)
		if len(args) > 2 {
			args[1] = result
			return evalDiv(vm, args[1:])
		} else {
			return result
		}
	} else {
		return vm.RuntimeExceptionf("value of type %T does not support /", args[0])
	}
}

func evalDef(vm *VM, args []Value) Value {
	if len(args) < 2 {
		return vm.RuntimeExceptionf("def needs at least two arguments")
	}
	name := args[0]
	value := vm.Eval(args[1])
	vm.curlet[name] = value
	return value
}

func evalDo(vm *VM, args []Value) (result Value) {
	for _, form := range args {
		result = vm.Eval(form)
		if IsRuntimeException(result) {
			break
		}
	}
	return result
}

func evalLet(vm *VM, args []Value) Value {
	if len(args) == 0 {
		return nil
	}
	bindings, ok := args[0].(Vector)
	if !ok {
		return vm.RuntimeExceptionf("let expects bindings in a vector, got %T", args[0])
	}
	if len(bindings)%2 != 0 {
		return vm.RuntimeExceptionf("let bindings should contain even number of keys and values: %v", args[0])
	}
	let := make(Map, len(bindings)/2+1)
	let[symProto] = vm.curlet
	i := 0
	for i < len(bindings) {
		k := bindings[i]
		if _, ok := k.(*Symbol); !ok {
			return vm.RuntimeExceptionf("let binding keys should be symbols, got: %v", k)
		}
		i++
		v := vm.Eval(bindings[i])
		if IsRuntimeException(v) {
			return vm.RuntimeExceptionf("failed to evaluate let binding %v: %v", bindings[i], v)
		}
		i++
		let[k] = v
	}
	return vm.Inlet(let, func() Value {
		return evalDo(vm, args[1:])
	})
}

func evalAssert(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("assert expects one argument, got %d", len(args))
	}
	expr := args[0]
	value := vm.Eval(expr)
	if IsRuntimeException(value) {
		return value
	}
	if value != TrueValue {
		if l, ok := expr.(List); ok && len(l) == 3 && l[0] == internSymbol("=") {
			actual := vm.Eval(l[1])
			if IsRuntimeException(actual) {
				return actual
			}
			expected := vm.Eval(l[2])
			if IsRuntimeException(expected) {
				return expected
			}
			return vm.RuntimeExceptionf("assertion failed: %s: %s != %s", Repr(expr), Repr(actual), Repr(expected))
		}
		return vm.RuntimeExceptionf("assertion failed: %s", Repr(expr))
	}
	return value
}

func evalQuote(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("quote expects one argument, got %d", len(args))
	}
	return args[0]
}

func resolveUnquoteSplicing(vm *VM, v Value) Value {
	if l, ok := v.(List); ok {
		if sym, ok := l[0].(*Symbol); ok && sym == symUnquoteSplicing {
			valueToSplice := vm.Eval(l[1])
			switch v := valueToSplice.(type) {
			case List:
				return v
			case Vector:
				return List(v)
			default:
				return vm.RuntimeExceptionf("expected list or vector result from unquote-splicing, got %T", valueToSplice)
			}
		}
	}
	return nil
}

func qq(vm *VM, arg Value) Value {
	switch v := arg.(type) {
	case *Cons:
		car := qq(vm, v.Car)
		if IsRuntimeException(car) {
			return car
		}
		cdr := qq(vm, v.Cdr)
		if IsRuntimeException(cdr) {
			return cdr
		}
		return &Cons{
			Car: car,
			Cdr: cdr,
		}
	case List:
		if len(v) == 0 {
			return arg
		}
		if sym, ok := v[0].(*Symbol); ok && sym == symUnquote {
			return vm.Eval(v[1])
		}
		result := make(List, 0, len(v))
		for _, item := range v {
			qqResult := qq(vm, item)
			if IsRuntimeException(qqResult) {
				return qqResult
			}
			if valueToSplice := resolveUnquoteSplicing(vm, qqResult); valueToSplice != nil {
				if IsRuntimeException(valueToSplice) {
					return valueToSplice
				}
				for _, v := range valueToSplice.(List) {
					result = append(result, v)
				}
			} else {
				result = append(result, qqResult)
			}
		}
		return result
	case Vector:
		if len(v) == 0 {
			return arg
		}
		result := make(Vector, 0, len(v))
		for _, item := range v {
			qqResult := qq(vm, item)
			if IsRuntimeException(qqResult) {
				return qqResult
			}
			if valueToSplice := resolveUnquoteSplicing(vm, qqResult); valueToSplice != nil {
				if IsRuntimeException(valueToSplice) {
					return valueToSplice
				}
				for _, v := range valueToSplice.(List) {
					result = append(result, v)
				}
			} else {
				result = append(result, qqResult)
			}
		}
		return result
	case Map:
		result := make(Map, len(v))
		for k, v := range v {
			qqk := qq(vm, k)
			if IsRuntimeException(qqk) {
				return qqk
			}
			qqv := qq(vm, v)
			if IsRuntimeException(qqv) {
				return qqv
			}
			result[qqk] = qqv
		}
		return result
	default:
		return arg
	}
}

func evalQuasiQuote(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return vm.RuntimeExceptionf("quasiquote expects one argument, got %d", len(args))
	}
	return qq(vm, args[0])
}

func init() {
	RegisterModule("langsam", func(vm *VM) error {
		vm.defineValue("nil", nil)
		vm.defineValue("true", TrueValue)
		vm.defineValue("false", FalseValue)
		vm.defineSpecialForm("assert", evalAssert)
		vm.defineSpecialForm("quote", evalQuote)
		vm.defineSpecialForm("quasiquote", evalQuasiQuote)
		vm.defineSpecialForm("fn", evalFn)
		vm.defineSpecialForm("macro", evalMacro)
		vm.defineSpecialForm("def", evalDef)
		vm.defineSpecialForm("do", evalDo)
		vm.defineSpecialForm("let", evalLet)
		vm.defineNativeFn("Nil", NilType)
		vm.defineNativeFn("Error", ErrorType)
		vm.defineNativeFn("Boolean", BooleanType)
		vm.defineNativeFn("Integer", IntegerType)
		vm.defineNativeFn("Float", FloatType)
		vm.defineNativeFn("String", StringType)
		vm.defineNativeFn("Symbol", SymbolType)
		vm.defineNativeFn("Keyword", KeywordType)
		vm.defineNativeFn("Cons", ConsType)
		vm.defineNativeFn("List", ListType)
		vm.defineNativeFn("Vector", VectorType)
		vm.defineNativeFn("Map", MapType)
		vm.defineNativeFn("Function", FunctionType)
		vm.defineNativeFn("=", evalEq)
		vm.defineNativeFn("!=", evalNotEq)
		vm.defineNativeFn("+", evalAdd)
		vm.defineNativeFn("-", evalSub)
		vm.defineNativeFn("*", evalMul)
		vm.defineNativeFn("/", evalDiv)
		vm.defineNativeFn("error", evalError)
		vm.defineNativeFn("str", evalStr)
		vm.defineNativeFn("repr", evalRepr)
		vm.defineNativeFn("eval", evalEval)
		vm.defineNativeFn("type", evalType)
		vm.defineNativeFn("len", evalLen)
		vm.defineNativeFn("deref", evalDeref)
		vm.defineNativeFn("cons", evalCons)
		vm.defineNativeFn("car", evalCar)
		vm.defineNativeFn("cdr", evalCdr)
		if result := vm.LoadString(langsamL); IsError(result) {
			return result.(error)
		}
		return nil
	})
}

func NewVM() (vm *VM, err error) {
	vm = &VM{}
	var importResult Value
	vm.once.Do(func() {
		vm.rootlet = make(Map)
		importResult = vm.Inlet(vm.rootlet, func() Value {
			for name, importModule := range registeredModules {
				if err := importModule(vm); err != nil {
					return Errorf("import failed for module %s: %v", name, err)
				}
			}
			return nil
		})
	})
	if importResult != nil {
		return nil, importResult.(Error).Payload.(error)
	}
	vm.curlet = make(Map)
	vm.curlet[symProto] = vm.rootlet
	return vm, nil
}
