//go:build unix

package main

import (
	"os/exec"
	"syscall"
)

// A terminal interrupt should reach the adapter first, so the program can
// finish its event loop on EOF even when the user presses Ctrl-C.
func configureChild(child *exec.Cmd) {
	child.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
}
