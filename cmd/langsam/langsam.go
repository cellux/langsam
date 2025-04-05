package main

import (
	"fmt"
	"github.com/cellux/langsam"
	"os"
)

func die(format string, args ...any) {
	fmt.Fprintf(os.Stderr, format, args...)
	os.Exit(1)
}

func main() {
	vm, err := langsam.NewVM()
	if err != nil {
		die("Error creating VM: %v\n", err)
	}
	if len(os.Args) > 1 {
		for _, arg := range os.Args[1:] {
			f, err := os.Open(arg)
			if err != nil {
				die("Error opening %s: %v\n", arg, err)
			}
			defer f.Close()
			if result := vm.Load(f); langsam.IsError(result) {
				die("Error while loading %s: %v\n", arg, result)
			}
		}
	} else {
		if result := vm.Load(os.Stdin); langsam.IsError(result) {
			die("Error while loading from stdin: %v\n", result)
		}
	}
}
