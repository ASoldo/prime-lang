//go:build !unix

package main

import "os/exec"

func configureChild(_ *exec.Cmd) {}
