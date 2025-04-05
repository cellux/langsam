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

type Error error

type Boolean bool
type Integer int64
type Float float64
type String string

type Symbol struct {
	Name string
}

type Keyword struct {
	Name *Symbol
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
	Len(vm *VM) Integer
}

type IEval interface {
	Eval(vm *VM) Value
}

// predefined symbols

var symAmpersand = internSymbol("&")

var symProto = internSymbol("%proto")

var symEqual = internSymbol("=")

var symQuote = internSymbol("quote")
var symQuasiQuote = internSymbol("quasiquote")

var symUnquote = internSymbol("unquote")
var symUnquoteSplicing = internSymbol("unquote-splicing")

// Value

func stringify(v Value) string {
	if stringer, ok := v.(fmt.Stringer); ok {
		return stringer.String()
	} else {
		return fmt.Sprintf("%v", v)
	}
}

// Error

func IsError(v Value) bool {
	_, ok := v.(error)
	return ok
}

// Nil

var symNilType = internSymbol("Nil")

func NilType(vm *VM, args []Value) Value {
	return nil
}

// Boolean

var symBooleanType = internSymbol("Boolean")

func BooleanType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return fmt.Errorf("Boolean expects one argument, got %d", len(args))
	}
	switch args[0].(type) {
	case Boolean:
		return args[0]
	default:
		return fmt.Errorf("cannot cast value of type %T to Boolean", args[0])
	}
}

var TrueValue = Boolean(true)
var FalseValue = Boolean(false)

func (b Boolean) Type(vm *VM) Value {
	return vm.rootEnv[symBooleanType]
}

func (b Boolean) Eval(vm *VM) Value {
	return b
}

func (b1 Boolean) Cmp(vm *VM, v2 Value) Value {
	if b2, ok := v2.(Boolean); ok && b1 == b2 {
		return Integer(0)
	} else {
		return fmt.Errorf("cannot compare Boolean with value of type %T", v2)
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
		return fmt.Errorf("Integer expects one argument, got %d", len(args))
	}
	switch args[0].(type) {
	case Integer:
		return args[0]
	case Float:
		return AsInteger(args[0])
	default:
		return fmt.Errorf("cannot cast value of type %T to Integer", args[0])
	}
}

func (i Integer) Type(vm *VM) Value {
	return vm.rootEnv[symIntegerType]
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
		return fmt.Errorf("cannot compare Integer with value of type %T", v2)
	}
}

func (i Integer) Eval(vm *VM) Value {
	return i
}

func AsInteger(v Value) Integer {
	switch v := v.(type) {
	case Integer:
		return v
	case Float:
		return Integer(int64(math.Round(float64(v))))
	default:
		return Integer(0)
	}
}

func (i Integer) Add(vm *VM, rhs Value) Value {
	return Integer(i + AsInteger(rhs))
}

func (i Integer) Sub(vm *VM, rhs Value) Value {
	return Integer(i - AsInteger(rhs))
}

func (i Integer) Mul(vm *VM, rhs Value) Value {
	return Integer(i * AsInteger(rhs))
}

func (i Integer) Div(vm *VM, rhs Value) Value {
	return Integer(i / AsInteger(rhs))
}

func (i Integer) String() string {
	return fmt.Sprintf("%d", i)
}

// Float

var symFloatType = internSymbol("Float")

func FloatType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return fmt.Errorf("Float expects one argument, got %d", len(args))
	}
	switch args[0].(type) {
	case Float:
		return args[0]
	case Integer:
		return AsFloat(args[0])
	default:
		return fmt.Errorf("cannot cast value of type %T to Float", args[0])
	}
}

func (f Float) Type(vm *VM) Value {
	return vm.rootEnv[symFloatType]
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
		return fmt.Errorf("cannot compare Float with value of type %T", v2)
	}
}

func AsFloat(v Value) Float {
	switch v := v.(type) {
	case Integer:
		return Float(float64(v))
	case Float:
		return v
	default:
		return Float(0.0)
	}
}

func (f Float) Eval(vm *VM) Value {
	return f
}

func (f Float) Add(vm *VM, rhs Value) Value {
	return Float(f + AsFloat(rhs))
}

func (f Float) Sub(vm *VM, rhs Value) Value {
	return Float(f - AsFloat(rhs))
}

func (f Float) Mul(vm *VM, rhs Value) Value {
	return Float(f * AsFloat(rhs))
}

func (f Float) Div(vm *VM, rhs Value) Value {
	return Float(f / AsFloat(rhs))
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
	var sb strings.Builder
	for _, v := range args {
		sb.WriteString(stringify(v))
	}
	return String(sb.String())
}

func (s String) Type(vm *VM) Value {
	return vm.rootEnv[symStringType]
}

func (s1 String) Cmp(vm *VM, v2 Value) Value {
	if s2, ok := v2.(String); ok {
		lhs := string(s1)
		rhs := string(s2)
		return Integer(strings.Compare(lhs, rhs))
	} else {
		return fmt.Errorf("cannot compare String with value of type %T", v2)
	}
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

// Symbol

var symSymbolType = internSymbol("Symbol")

func SymbolType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return fmt.Errorf("Symbol expects one argument, got %d", len(args))
	}
	switch v := args[0].(type) {
	case *Symbol:
		return args[0]
	case String:
		return internSymbol(string(v))
	default:
		return fmt.Errorf("cannot cast value of type %T to Symbol", args[0])
	}
}

func (sym *Symbol) Type(vm *VM) Value {
	return vm.rootEnv[symSymbolType]
}

func (sym1 *Symbol) Cmp(vm *VM, v2 Value) Value {
	if sym2, ok := v2.(*Symbol); ok {
		if sym1 == sym2 {
			return Integer(0)
		} else {
			return Integer(1)
		}
	} else {
		return fmt.Errorf("cannot compare Symbol with value of type %T", v2)
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
		return fmt.Errorf("Keyword expects one argument, got %d", len(args))
	}
	switch v := args[0].(type) {
	case Keyword:
		return args[0]
	case String:
		return internKeyword(string(v))
	case *Symbol:
		return internKeyword(v.Name)
	default:
		return fmt.Errorf("cannot cast value of type %T to Keyword", args[0])
	}
}

func (kw Keyword) Type(vm *VM) Value {
	return vm.rootEnv[symKeywordType]
}

func (kw1 Keyword) Cmp(vm *VM, v2 Value) Value {
	if kw2, ok := v2.(Keyword); ok {
		if kw1 == kw2 {
			return Integer(0)
		} else {
			return Integer(1)
		}
	} else {
		return fmt.Errorf("cannot compare Keyword with value of type %T", v2)
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
	return ":" + kw.Name.Name
}

// Cons

var symConsType = internSymbol("Cons")

func ConsType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return fmt.Errorf("Cons expects a single argument, got %d", len(args))
	}
	switch args[0].(type) {
	case *Cons:
		return args[0]
	default:
		return fmt.Errorf("cannot cast value of type %T to Cons", args[0])
	}
}

func (c *Cons) Type(vm *VM) Value {
	return vm.rootEnv[symConsType]
}

func (c1 *Cons) Cmp(vm *VM, v2 Value) Value {
	if c2, ok := v2.(*Cons); ok {
		carCmpResult := vm.Cmp(c1.Car, c2.Car)
		if IsError(carCmpResult) {
			return carCmpResult
		}
		if carCmpResult.(Integer) != 0 {
			return carCmpResult
		}
		return vm.Cmp(c1.Cdr, c2.Cdr)
	} else {
		return fmt.Errorf("cannot compare Cons with value of type %T", v2)
	}
}

func (c *Cons) String() string {
	if c == nil {
		return "nil"
	}
	var sb strings.Builder
	sb.WriteByte('(')
	for c != nil {
		sb.WriteString(stringify(c.Car))
		if tail, ok := c.Cdr.(*Cons); ok {
			if tail != nil {
				sb.WriteString(" ")
			}
			c = tail
		} else {
			sb.WriteString(" . ")
			sb.WriteString(stringify(c.Cdr))
			break
		}
	}
	sb.WriteByte(')')
	return sb.String()
}

func evalCons(vm *VM, args []Value) Value {
	if len(args) != 2 {
		return fmt.Errorf("cons expects two arguments, got %d", len(args))
	}
	return &Cons{
		Car: args[0],
		Cdr: args[1],
	}
}

func evalCar(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return fmt.Errorf("car expects one argument, got %d", len(args))
	}
	if c, ok := args[0].(*Cons); ok {
		return c.Car
	} else {
		return fmt.Errorf("car expects a Cons argument, got %T", args[0])
	}
}

func evalCdr(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return fmt.Errorf("cdr expects one argument, got %d", len(args))
	}
	if c, ok := args[0].(*Cons); ok {
		return c.Cdr
	} else {
		return fmt.Errorf("cdr expects a Cons argument, got %T", args[0])
	}
}

// List

func readList(src io.ByteScanner) (Value, error) {
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

func (l1 List) Cmp(vm *VM, v2 Value) Value {
	if l2, ok := v2.(List); ok {
		if len(l1) < len(l2) {
			return Integer(-1)
		} else if len(l1) > len(l2) {
			return Integer(1)
		} else {
			for i := range l1 {
				result := vm.Cmp(l1[i], l2[i])
				if IsError(result) {
					return result
				}
				if result.(Integer) != 0 {
					return result
				}
			}
			return Integer(0)
		}
	} else {
		return fmt.Errorf("cannot compare List with value of type %T", v2)
	}
}

func (l List) Eval(vm *VM) Value {
	if len(l) == 0 {
		return fmt.Errorf("cannot evaluate empty list")
	}
	v0 := vm.Eval(l[0])
	if IsError(v0) {
		return v0
	}
	if v0 == nil {
		return fmt.Errorf("'%v' is nil", l[0])
	}
	if f, ok := v0.(ICall); ok {
		return f.Call(vm, l[1:])
	} else {
		return fmt.Errorf("head of list (%v) has type %T which is not callable", l[0], v0)
	}
}

func (l List) String() string {
	var sb strings.Builder
	sb.WriteByte('(')
	for i := range len(l) {
		if i > 0 {
			sb.WriteString(" ")
		}
		sb.WriteString(stringify(l[i]))
	}
	sb.WriteByte(')')
	return sb.String()
}

// Vector

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

func (v Vector) Eval(vm *VM) Value {
	ev := make(Vector, len(v))
	for i := range len(v) {
		if ev[i] = vm.Eval(v[i]); IsError(ev[i]) {
			return ev[i]
		}
	}
	return ev
}

func (v Vector) String() string {
	var sb strings.Builder
	sb.WriteByte('[')
	for i := range len(v) {
		if i > 0 {
			sb.WriteString(" ")
		}
		sb.WriteString(stringify(v[i]))
	}
	sb.WriteByte(']')
	return sb.String()
}

// Map

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

func (m Map) Eval(vm *VM) Value {
	em := make(Map, len(m))
	for k, v := range m {
		var ek, ev Value
		if ek = vm.Eval(k); IsError(ek) {
			return ek
		}
		if ev = vm.Eval(v); IsError(ev) {
			return ev
		}
		em[ek] = ev
	}
	return em
}

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
	var sb strings.Builder
	sb.WriteByte('{')
	i := 0
	for k, v := range m {
		if i > 0 {
			sb.WriteString(" ")
		}
		sb.WriteString(stringify(k))
		sb.WriteString(" ")
		sb.WriteString(stringify(v))
		i++
	}
	sb.WriteByte('}')
	return sb.String()
}

// Function

var symFunctionType = internSymbol("Function")

func FunctionType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return fmt.Errorf("Function expects one argument, got %d", len(args))
	}
	switch arg := args[0].(type) {
	case *Function:
		return args[0]
	case Map:
		m := arg
		f := &Function{
			EvalArgs:   true,
			EvalResult: false,
		}
		if v, ok := m[internKeyword("name")]; ok {
			if name, ok := v.(*Symbol); ok {
				f.Name = name
			} else {
				return fmt.Errorf("expected symbol at :name, got %T", v)
			}
		}
		if v, ok := m[internKeyword("param-names")]; ok {
			if paramNameVector, ok := v.(Vector); ok {
				paramNames := make([]*Symbol, len(paramNameVector))
				for i := range len(paramNames) {
					if paramName, ok := paramNameVector[i].(*Symbol); ok {
						paramNames[i] = paramName
					} else {
						return fmt.Errorf("parameter names must be symbols, got %v", paramNameVector[i])
					}
				}
				f.ParamNames = paramNames
			} else {
				return fmt.Errorf("expected vector of symbols at :param-names, got %T", v)
			}
		}
		if v, ok := m[internKeyword("rest-name")]; ok {
			if restName, ok := v.(*Symbol); ok {
				f.RestName = restName
			} else {
				return fmt.Errorf("expected symbol at :rest-name, got %T", v)
			}
		}
		if v, ok := m[internKeyword("eval-args?")]; ok {
			if evalArgs, ok := v.(Boolean); ok {
				f.EvalArgs = bool(evalArgs)
			} else {
				return fmt.Errorf("expected boolean at :eval-args?, got %T", v)
			}
		}
		if v, ok := m[internKeyword("eval-result?")]; ok {
			if evalResult, ok := v.(Boolean); ok {
				f.EvalResult = bool(evalResult)
			} else {
				return fmt.Errorf("expected boolean at :eval-result?, got %T", v)
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
				return fmt.Errorf("expected list or native fn at :body, got %T", v)
			}
		}
		return f
	default:
		return fmt.Errorf("cannot cast value of type %T to Function", args[0])
	}
}

func (b *Function) Type(vm *VM) Value {
	return vm.rootEnv[symFunctionType]
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
			if IsError(realArgs[i]) {
				return fmt.Errorf("%v failed to evaluate arg #%d: %v", f, i, realArgs[i])
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
		fenv := vm.ChildEnv(nBindings)
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
		result = vm.InEnv(fenv, func() (result Value) {
			for _, form := range body {
				result = vm.Eval(form)
				if IsError(result) {
					break
				}
			}
			return result
		})
	case NativeFn:
		result = body(vm, realArgs)
	default:
		return fmt.Errorf("invalid function body, expected List or NativeFn, got %T", f.Body)
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

func (f1 *Function) Cmp(vm *VM, v2 Value) Value {
	if f2, ok := v2.(*Function); ok && f1 == f2 {
		return Integer(0)
	} else {
		return fmt.Errorf("cannot compare Function with value of type %T", v2)
	}
}

func (f *Function) String() string {
	return f.Name.Name
}

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
					return fmt.Errorf("there must be a single symbol after & in parameter list")
				}
				restName = paramSym
			} else if paramSym == restMarker {
				seenRestMarker = true
			} else {
				paramNames = append(paramNames, paramSym)
			}
		} else {
			return fmt.Errorf("parameters must be symbols")
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
			return readList(src)
		case b == '[':
			return readVector(src)
		case b == '{':
			return readMap(src)
		case b == ':':
			return readKeyword(src)
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
	rootEnv Map
	curEnv  Map
	modules map[string]string
}

func (vm *VM) Type(v Value) Value {
	if v == nil {
		return vm.rootEnv[symNilType]
	}
	if i, ok := v.(IType); ok {
		return i.Type(vm)
	} else {
		return fmt.Errorf("type of %v cannot be determined", v)
	}
}

func (vm *VM) Len(v Value) Value {
	if i, ok := v.(ILen); ok {
		return i.Len(vm)
	} else {
		return fmt.Errorf("value of type %T does not provide length", v)
	}
}

func (vm *VM) Cmp(v1 Value, v2 Value) Value {
	if v1 == nil {
		if v2 == nil {
			return Integer(0)
		} else {
			return Integer(1)
		}
	}
	if lhs, ok := v1.(ICmp); ok {
		return lhs.Cmp(vm, v2)
	} else {
		return fmt.Errorf("value of type %T does not support comparison", v1)
	}
}

func (vm *VM) Eval(v Value) Value {
	if v == nil {
		return nil
	}
	if i, ok := v.(IEval); ok {
		return i.Eval(vm)
	} else {
		return fmt.Errorf("value of type %T does not support evaluation", v)
	}
}

func (vm *VM) Lookup(key Value) Value {
	return vm.curEnv.Lookup(key)
}

func (vm *VM) Load(rdr io.Reader) (result Value) {
	src := bufio.NewReader(rdr)
	for {
		form, err := read(src)
		if err != nil {
			if err == io.EOF {
				break
			}
			return fmt.Errorf("read failed: %v", err)
		}
		result = vm.Eval(form)
		if IsError(result) {
			return fmt.Errorf("%v\nevaluation failed: %v", form, result)
		}
	}
	return result
}

func (vm *VM) LoadString(s string) Value {
	return vm.Load(strings.NewReader(s))
}

func (vm *VM) defineValue(name string, value Value) {
	sym := internSymbol(name)
	vm.curEnv[sym] = value
}

func (vm *VM) defineNativeFn(name string, fn NativeFn) *Function {
	sym := internSymbol(name)
	f := &Function{
		Name:       sym,
		EvalArgs:   true,
		EvalResult: false,
		Body:       fn,
	}
	vm.curEnv[sym] = f
	return f
}

func (vm *VM) defineSpecialForm(name string, fn NativeFn) *Function {
	return vm.defineNativeFn(name, fn).WithEvalArgs(false).WithEvalResult(false)
}

func (vm *VM) InEnv(env Map, f func() Value) Value {
	oldEnv := vm.curEnv
	vm.curEnv = env
	result := f()
	vm.curEnv = oldEnv
	return result
}

func (vm *VM) ChildEnv(size int) Map {
	var m Map
	if size == 0 {
		m = make(Map)
	} else {
		m = make(Map, size+1)
	}
	m[symProto] = vm.curEnv
	return m
}

type ImportFn func(vm *VM) error

var registeredModules = make(map[string]ImportFn)

func RegisterModule(name string, importFn ImportFn) {
	registeredModules[name] = importFn
}

func evalType(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return fmt.Errorf("type expects one argument, got %d", len(args))
	}
	return vm.Type(args[0])
}

func evalLen(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return fmt.Errorf("len expects one argument, got %d", len(args))
	}
	return vm.Len(args[0])
}

func evalEval(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return fmt.Errorf("eval expects one argument, got %d", len(args))
	}
	return vm.Eval(args[0])
}

func evalEqual(vm *VM, args []Value) Value {
	if len(args) == 0 {
		return fmt.Errorf("= expects at least one argument")
	}
	if len(args) == 1 {
		return TrueValue
	}
	result := vm.Cmp(args[0], args[1])
	if IsError(result) {
		return result
	}
	if result.(Integer) != 0 {
		return FalseValue
	}
	if len(args) > 2 {
		return evalEqual(vm, args[1:])
	} else {
		return TrueValue
	}
}

func evalAdd(vm *VM, args []Value) Value {
	if len(args) == 0 {
		return fmt.Errorf("+ expects at least one argument")
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
		return fmt.Errorf("value of type %T does not support +", args[0])
	}
}

func evalSub(vm *VM, args []Value) Value {
	if len(args) == 0 {
		return fmt.Errorf("- expects at least one argument")
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
		return fmt.Errorf("value of type %T does not support -", args[0])
	}
}

func evalMul(vm *VM, args []Value) Value {
	if len(args) == 0 {
		return fmt.Errorf("* expects at least one argument")
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
		return fmt.Errorf("value of type %T does not support *", args[0])
	}
}

func evalDiv(vm *VM, args []Value) Value {
	if len(args) == 0 {
		return fmt.Errorf("/ expects at least one argument")
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
		return fmt.Errorf("value of type %T does not support /", args[0])
	}
}

func evalDef(vm *VM, args []Value) Value {
	if len(args) < 2 {
		return fmt.Errorf("def needs at least two arguments")
	}
	name := args[0]
	value := vm.Eval(args[1])
	if IsError(value) {
		return value
	}
	vm.curEnv[name] = value
	return value
}

func evalDo(vm *VM, args []Value) (result Value) {
	for _, form := range args {
		result = vm.Eval(form)
		if IsError(result) {
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
		return fmt.Errorf("let expects bindings in a vector, got %T", args[0])
	}
	if len(bindings)%2 != 0 {
		return fmt.Errorf("let bindings should contain even number of keys and values: %v", args[0])
	}
	letenv := make(Map, len(bindings)/2+1)
	letenv[symProto] = vm.curEnv
	i := 0
	for i < len(bindings) {
		k := bindings[i]
		if _, ok := k.(*Symbol); !ok {
			return fmt.Errorf("let binding keys should be symbols, got: %v", k)
		}
		i++
		v := vm.Eval(bindings[i])
		if IsError(v) {
			return fmt.Errorf("failed to evaluate let binding %v: %v", bindings[i], v)
		}
		i++
		letenv[k] = v
	}
	return vm.InEnv(letenv, func() Value {
		return evalDo(vm, args[1:])
	})
}

func evalAssert(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return fmt.Errorf("assert expects one argument, got %d", len(args))
	}
	expr := args[0]
	value := vm.Eval(expr)
	if IsError(value) {
		return value
	}
	if value != TrueValue {
		if l, ok := expr.(List); ok && len(l) == 3 && l[0] == symEqual {
			actual := vm.Eval(l[1])
			if IsError(actual) {
				return actual
			}
			expected := vm.Eval(l[2])
			if IsError(expected) {
				return expected
			}
			return fmt.Errorf("assertion failed: %v: %v != %v", expr, actual, expected)
		}
		return fmt.Errorf("assertion failed: %v", expr)
	}
	return value
}

func evalQuote(vm *VM, args []Value) Value {
	if len(args) != 1 {
		return fmt.Errorf("quote expects one argument, got %d", len(args))
	}
	return args[0]
}

func handleUnquoteSplicing(vm *VM, v Value) Value {
	if l, ok := v.(List); ok {
		if sym, ok := l[0].(*Symbol); ok && sym == symUnquoteSplicing {
			return vm.Eval(l[1])
		}
	}
	return nil
}

func qq(vm *VM, arg Value) Value {
	switch v := arg.(type) {
	case Cons:
		car := qq(vm, v.Car)
		if IsError(car) {
			return car
		}
		cdr := qq(vm, v.Cdr)
		if IsError(cdr) {
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
			if IsError(qqResult) {
				return qqResult
			}
			if l := handleUnquoteSplicing(vm, qqResult); l != nil {
				if IsError(l) {
					return l
				}
				for _, v := range l.(List) {
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
			if IsError(qqResult) {
				return qqResult
			}
			if l := handleUnquoteSplicing(vm, qqResult); l != nil {
				if IsError(l) {
					return l
				}
				for _, v := range l.(List) {
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
			if IsError(qqk) {
				return qqk
			}
			qqv := qq(vm, v)
			if IsError(qqv) {
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
		return fmt.Errorf("quasiquote expects one argument, got %d", len(args))
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
		vm.defineNativeFn("Boolean", BooleanType)
		vm.defineNativeFn("Integer", IntegerType)
		vm.defineNativeFn("Float", FloatType)
		vm.defineNativeFn("String", StringType)
		vm.defineNativeFn("Symbol", SymbolType)
		vm.defineNativeFn("Keyword", KeywordType)
		vm.defineNativeFn("Cons", ConsType)
		vm.defineNativeFn("Function", FunctionType)
		vm.defineNativeFn("=", evalEqual)
		vm.defineNativeFn("+", evalAdd)
		vm.defineNativeFn("-", evalSub)
		vm.defineNativeFn("*", evalMul)
		vm.defineNativeFn("/", evalDiv)
		vm.defineNativeFn("eval", evalEval)
		vm.defineNativeFn("type", evalType)
		vm.defineNativeFn("len", evalLen)
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
		vm.rootEnv = make(Map)
		importResult = vm.InEnv(vm.rootEnv, func() Value {
			for name, importModule := range registeredModules {
				if err := importModule(vm); err != nil {
					return fmt.Errorf("import failed for module %s: %v", name, err)
				}
			}
			return nil
		})
	})
	if importResult != nil {
		return nil, importResult.(error)
	}
	vm.curEnv = make(Map)
	vm.curEnv[symProto] = vm.rootEnv
	return vm, nil
}
