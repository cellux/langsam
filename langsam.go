package langsam

import (
	"bufio"
	_ "embed"
	"fmt"
	"io"
	"math"
	"strconv"
	"strings"
	"sync"
)

//go:embed langsam.l
var langsamL string

// value types

type Value any

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

type NativeFn func(vm *VM, args []Value) (Value, error)

// value interfaces

type IAdd interface {
	Add(vm *VM, args []Value) (Value, error)
}

type ISub interface {
	Sub(vm *VM, args []Value) (Value, error)
}

type IMul interface {
	Mul(vm *VM, args []Value) (Value, error)
}

type IDiv interface {
	Div(vm *VM, args []Value) (Value, error)
}

type ICall interface {
	Call(vm *VM, args []Value) (Value, error)
}

type IEval interface {
	Eval(vm *VM) (Value, error)
}

// value constants

var TrueValue = Boolean(true)
var FalseValue = Boolean(false)

// predefined symbols

var symAmpersand = internSymbol("&")

var symProto = internSymbol("%proto")

var symEqual = internSymbol("=")

var symQuote = internSymbol("quote")
var symQuasiQuote = internSymbol("quasiquote")

var symUnquote = internSymbol("unquote")
var symUnquoteSplicing = internSymbol("unquote-splicing")

// predefined keywords

var kwEvalArgs = internKeyword("eval-args?")
var kwEvalResult = internKeyword("eval-result?")

// Value

func stringify(v Value) string {
	if stringer, ok := v.(fmt.Stringer); ok {
		return stringer.String()
	} else {
		return fmt.Sprintf("%v", v)
	}
}

// Boolean

func (b Boolean) String() string {
	if b == TrueValue {
		return "true"
	} else {
		return "false"
	}
}

func (b Boolean) Eval(vm *VM) (Value, error) {
	return b, nil
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

func (i Integer) String() string {
	return fmt.Sprintf("%d", i)
}

func (i Integer) Eval(vm *VM) (Value, error) {
	return i, nil
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

func (i Integer) Add(vm *VM, args []Value) (Value, error) {
	result := i
	for i := range len(args) {
		result += AsInteger(args[i])
	}
	return result, nil
}

func (i Integer) Sub(vm *VM, args []Value) (Value, error) {
	result := i
	for i := range len(args) {
		result -= AsInteger(args[i])
	}
	return result, nil
}

func (i Integer) Mul(vm *VM, args []Value) (Value, error) {
	result := i
	for i := range len(args) {
		result *= AsInteger(args[i])
	}
	return result, nil
}

func (i Integer) Div(vm *VM, args []Value) (Value, error) {
	result := i
	for i := range len(args) {
		result /= AsInteger(args[i])
	}
	return result, nil
}

// Float

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

func (f Float) String() string {
	s := strconv.FormatFloat(float64(f), 'f', -1, 64)
	if !strings.ContainsRune(s, '.') {
		s += ".0"
	}
	return s
}

func (f Float) Eval(vm *VM) (Value, error) {
	return f, nil
}

func (f Float) Add(vm *VM, args []Value) (Value, error) {
	result := f
	for i := range len(args) {
		result += AsFloat(args[i])
	}
	return result, nil
}

func (f Float) Sub(vm *VM, args []Value) (Value, error) {
	result := f
	for i := range len(args) {
		result -= AsFloat(args[i])
	}
	return result, nil
}

func (f Float) Mul(vm *VM, args []Value) (Value, error) {
	result := f
	for i := range len(args) {
		result *= AsFloat(args[i])
	}
	return result, nil
}

func (f Float) Div(vm *VM, args []Value) (Value, error) {
	result := f
	for i := range len(args) {
		result /= AsFloat(args[i])
	}
	return result, nil
}

// String

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

func (s String) String() string {
	return fmt.Sprintf("%#v", s)
}

func (s String) Eval(vm *VM) (Value, error) {
	return s, nil
}

// Symbol

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

func (sym *Symbol) String() string {
	return sym.Name
}

func (sym *Symbol) Eval(vm *VM) (Value, error) {
	return vm.Lookup(sym), nil
}

// Keyword

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

func (kw Keyword) String() string {
	return ":" + kw.Name.Name
}

func (kw Keyword) Eval(vm *VM) (Value, error) {
	return kw, nil
}

// Cons

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

func (l List) Eval(vm *VM) (Value, error) {
	if len(l) == 0 {
		return nil, fmt.Errorf("cannot evaluate empty list")
	}
	v0, err := vm.Eval(l[0])
	if err != nil {
		return nil, err
	}
	if f, ok := v0.(ICall); ok {
		return f.Call(vm, l[1:])
	} else {
		return nil, fmt.Errorf("head of list (%v) has type %T which is not callable", l[0], v0)
	}
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

func (v Vector) String() string {
	var sb strings.Builder
	sb.WriteByte('[')
	for i := range len(v) {
		if i > 0 {
			sb.WriteString(" ")
		}
		sb.WriteString(stringify(v[i]))
	}
	sb.WriteByte(')')
	return sb.String()
}

func (v Vector) Eval(vm *VM) (result Value, err error) {
	ev := make(Vector, len(v))
	for i := range len(v) {
		if ev[i], err = vm.Eval(v[i]); err != nil {
			return nil, err
		}
	}
	return ev, nil
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

func (m Map) Eval(vm *VM) (result Value, err error) {
	em := make(Map, len(m))
	for k, v := range m {
		var ek, ev Value
		if ek, err = vm.Eval(k); err != nil {
			return nil, err
		}
		if ev, err = vm.Eval(v); err != nil {
			return nil, err
		}
		em[ek] = ev
	}
	return em, nil
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

// Function

func (f *Function) String() string {
	return f.Name.Name
}

func (f *Function) Eval(vm *VM) (Value, error) {
	return f, nil
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

func (f *Function) Call(vm *VM, args []Value) (result Value, err error) {
	var realArgs []Value
	if f.EvalArgs {
		realArgs = make([]Value, len(args))
		for i := range len(args) {
			realArgs[i], err = vm.Eval(args[i])
			if err != nil {
				return nil, fmt.Errorf("%v failed to evaluate arg #%d: %v", f, i, err)
			}
		}
	} else {
		realArgs = args
	}
	switch body := f.Body.(type) {
	case Vector:
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
		result, err = vm.InEnv(fenv, func() (Value, error) {
			for _, form := range body {
				result, err = vm.Eval(form)
				if err != nil {
					return nil, err
				}
			}
			return result, err
		})
	case NativeFn:
		result, err = body(vm, realArgs)
	default:
		return nil, fmt.Errorf("invalid function body, expected Vector or NativeFn, got %T", f.Body)
	}
	if err != nil {
		return nil, err
	}
	if f.EvalResult {
		return vm.Eval(result)
	} else {
		return result, nil
	}
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
				return List([]Value{symUnquote, form, nil}), nil
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

func (vm *VM) Eval(v Value) (Value, error) {
	if i, ok := v.(IEval); ok {
		return i.Eval(vm)
	} else {
		return nil, fmt.Errorf("value of type %T does not support evaluation", v)
	}
}

func (vm *VM) Lookup(key Value) Value {
	return vm.curEnv.Lookup(key)
}

func (vm *VM) Load(rdr io.Reader) (lastResult Value, error error) {
	src := bufio.NewReader(rdr)
	for {
		form, err := read(src)
		if err != nil {
			if err == io.EOF {
				break
			}
			return nil, fmt.Errorf("read failed: %v", err)
		}
		lastResult, err = vm.Eval(form)
		if err != nil {
			return nil, fmt.Errorf("%v\nevaluation failed: %v", form, err)
		}
	}
	return lastResult, nil
}

func (vm *VM) LoadString(s string) (Value, error) {
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

func (vm *VM) defineNativeMacro(name string, fn NativeFn) *Function {
	return vm.defineNativeFn(name, fn).WithEvalArgs(false)
}

func (vm *VM) InEnv(env Map, f func() (Value, error)) (Value, error) {
	oldEnv := vm.curEnv
	vm.curEnv = env
	result, err := f()
	vm.curEnv = oldEnv
	return result, err
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

func evalEqual(vm *VM, args []Value) (Value, error) {
	lhs := args[0]
	for i := 1; i < len(args); i++ {
		if args[i] != lhs {
			return FalseValue, nil
		}
	}
	return TrueValue, nil
}

func evalAdd(vm *VM, args []Value) (Value, error) {
	if len(args) == 0 {
		return nil, nil
	}
	if i, ok := args[0].(IAdd); ok {
		return i.Add(vm, args[1:])
	} else {
		return nil, fmt.Errorf("value of type %T does not support +", args[0])
	}
}

func evalSub(vm *VM, args []Value) (Value, error) {
	if len(args) == 0 {
		return nil, nil
	}
	if i, ok := args[0].(ISub); ok {
		return i.Sub(vm, args[1:])
	} else {
		return nil, fmt.Errorf("value of type %T does not support -", args[0])
	}
}

func evalMul(vm *VM, args []Value) (Value, error) {
	if len(args) == 0 {
		return nil, nil
	}
	if i, ok := args[0].(IMul); ok {
		return i.Mul(vm, args[1:])
	} else {
		return nil, fmt.Errorf("value of type %T does not support *", args[0])
	}
}

func evalDiv(vm *VM, args []Value) (Value, error) {
	if len(args) == 0 {
		return nil, nil
	}
	if i, ok := args[0].(IDiv); ok {
		return i.Div(vm, args[1:])
	} else {
		return nil, fmt.Errorf("value of type %T does not support /", args[0])
	}
}

func evalFn(vm *VM, args []Value) (Value, error) {
	var fnName *Symbol
	var options Map
	var paramForms Vector
	for len(args) > 0 && paramForms == nil {
		switch v := args[0].(type) {
		case *Symbol:
			fnName = v
		case Map:
			optionsValue, err := vm.Eval(v)
			if err != nil {
				return nil, err
			}
			options = optionsValue.(Map)
		case Vector:
			paramForms = v
		}
		args = args[1:]
	}
	var paramNames []*Symbol
	var restName *Symbol
	restMarker := symAmpersand // &
	seenRestMarker := false
	for i, paramForm := range paramForms {
		if paramSym, ok := paramForm.(*Symbol); ok {
			if seenRestMarker {
				if i != len(paramForms)-1 {
					return nil, fmt.Errorf("there must be a single symbol after & in parameter list")
				}
				restName = paramSym
			} else if paramSym == restMarker {
				seenRestMarker = true
			} else {
				paramNames = append(paramNames, paramSym)
			}
		} else {
			return nil, fmt.Errorf("parameters must be symbols")
		}
	}
	evalArgs := true
	evalResult := false
	if options != nil {
		if v, ok := options[kwEvalArgs]; ok {
			evalArgs = v.(bool)
		}
		if v, ok := options[kwEvalResult]; ok {
			evalResult = v.(bool)
		}
	}
	return &Function{
		Name:       fnName,
		ParamNames: paramNames,
		RestName:   restName,
		EvalArgs:   evalArgs,
		EvalResult: evalResult,
		Body:       Vector(args),
	}, nil
}

func evalDef(vm *VM, args []Value) (Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("def needs at least two arguments")
	}
	name := args[0]
	value, err := vm.Eval(args[1])
	if err != nil {
		return nil, err
	}
	vm.curEnv[name] = value
	return value, nil
}

func evalDo(vm *VM, args []Value) (result Value, err error) {
	for _, form := range args {
		result, err = vm.Eval(form)
		if err != nil {
			return nil, err
		}
	}
	return result, err
}

func evalLet(vm *VM, args []Value) (Value, error) {
	if len(args) == 0 {
		return nil, nil
	}
	bindings, ok := args[0].(Vector)
	if !ok {
		return nil, fmt.Errorf("let expects bindings in a vector, got %T", args[0])
	}
	if len(bindings)%2 != 0 {
		return nil, fmt.Errorf("let bindings should contain even number of keys and values: %v", args[0])
	}
	letenv := make(Map, len(bindings)/2+1)
	letenv[symProto] = vm.curEnv
	i := 0
	for i < len(bindings) {
		k := bindings[i]
		if _, ok := k.(*Symbol); !ok {
			return nil, fmt.Errorf("let binding keys should be symbols, got: %v", k)
		}
		i++
		v, err := vm.Eval(bindings[i])
		if err != nil {
			return nil, fmt.Errorf("failed to evaluate let binding value: %v", bindings[i])
		}
		i++
		letenv[k] = v
	}
	return vm.InEnv(letenv, func() (Value, error) {
		return evalDo(vm, args[1:])
	})
}

func evalAssert(vm *VM, args []Value) (Value, error) {
	expr := args[0]
	value, err := vm.Eval(expr)
	if err != nil {
		return nil, err
	}
	if value != TrueValue {
		if l, ok := expr.(List); ok && len(l) == 3 && l[0] == symEqual {
			actual, err := vm.Eval(l[1])
			if err != nil {
				return nil, err
			}
			expected, err := vm.Eval(l[2])
			if err != nil {
				return nil, err
			}
			return nil, fmt.Errorf("assertion failed: %v: %v != %v", expr, actual, expected)
		}
		return nil, fmt.Errorf("assertion failed: %v", expr)
	}
	return value, nil
}

func evalQuote(vm *VM, args []Value) (Value, error) {
	return args[0], nil
}

func evalQuasiQuote(vm *VM, args []Value) (Value, error) {
	// TODO
	return args[0], nil
}

func init() {
	RegisterModule("langsam", func(vm *VM) error {
		vm.defineValue("nil", nil)
		vm.defineValue("true", TrueValue)
		vm.defineValue("false", FalseValue)
		vm.defineNativeFn("=", evalEqual)
		vm.defineNativeFn("+", evalAdd)
		vm.defineNativeFn("-", evalSub)
		vm.defineNativeFn("*", evalMul)
		vm.defineNativeFn("/", evalDiv)
		vm.defineNativeMacro("assert", evalAssert)
		vm.defineNativeMacro("quote", evalQuote)
		vm.defineNativeMacro("quasiquote", evalQuasiQuote)
		vm.defineNativeMacro("fn", evalFn)
		vm.defineNativeMacro("def", evalDef)
		vm.defineNativeMacro("do", evalDo)
		vm.defineNativeMacro("let", evalLet)
		if _, err := vm.LoadString(langsamL); err != nil {
			return err
		}
		return nil
	})
}

func NewVM() (vm *VM, newError error) {
	vm = &VM{}
	vm.once.Do(func() {
		vm.rootEnv = make(Map)
		_, newError = vm.InEnv(vm.rootEnv, func() (Value, error) {
			for name, importModule := range registeredModules {
				if err := importModule(vm); err != nil {
					return nil, fmt.Errorf("import failed for module %s: %v", name, err)
				}
			}
			return nil, nil
		})
	})
	if newError != nil {
		return
	}
	vm.curEnv = make(Map)
	vm.curEnv[symProto] = vm.rootEnv
	return vm, nil
}
