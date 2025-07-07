# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is gym-hs, a Haskell reinforcement learning library which wraps [Gymnasium](https://github.com/Farama-Foundation/Gymnasium). It lets you call any Gymnasium code from haskell.

## Development Environment

The project uses a devcontainer based on the `htorch/hasktorch-jupyter:latest` image, which includes:
- Haskell toolchain with GHC and Cabal
- Hasktorch for tensor operations and neural networks
- Jupyter notebook support for interactive development
- Python integration for interoperability

## Common Development Commands

### Project Setup
- `cabal update` - Update package index
- `cabal build` - Build the project
- `cabal repl` - Start interactive REPL
- `cabal test` - Run test suite
- `cabal run <executable>` - Run a specific executable

### Development Workflow
- `cabal build --enable-tests` - Build with tests enabled
- `cabal test --test-show-details=direct` - Run tests with detailed output
- `cabal haddock` - Generate documentation
- `cabal clean` - Clean build artifacts

### Jupyter Integration
- Port 8888 is forwarded for Jupyter notebook access
- Use `jupyter notebook` or `jupyter lab` for interactive development
- IHaskell kernel should be available for Haskell notebooks

## Architecture Notes

### Expected Structure
- `src/` - Main library source code
- `app/` - Executable applications
- `test/` - Test suites
- `examples/` - Example usage and demos
- `notebooks/` - Jupyter notebooks for experimentation

### Key Components
- Environment interface compatible with OpenAI Gym
- State and action space definitions
- Reward functions and episode management
- Integration with Hasktorch for deep RL algorithms
- Python FFI bindings for interoperability

### Dependencies
- Hasktorch for tensor operations
- Vector and array libraries for efficient data structures
- Aeson for JSON serialization
- Random number generation libraries
- Testing frameworks (likely tasty or hspec)

## Development Tips

- Use `cabal repl` for interactive development and testing
- Leverage Hasktorch's tensor operations for performance-critical code
- Follow standard Haskell project structure conventions
- Use type-safe interfaces for environment definitions
- Consider lazy evaluation implications for RL training loops

## Publishing to Hackage

The package is configured for Hackage publishing with:
- Proper metadata in package.yaml
- MIT license file
- Comprehensive documentation
- Version bounds for dependencies
- Changelog following Keep a Changelog format

To publish:
1. `cabal check` - Validate package
2. `cabal sdist` - Create source distribution
3. `cabal upload --publish dist-newstyle/sdist/gym-hs-0.1.0.0.tar.gz` - Upload to Hackage

Required for publishing:
- Hackage account with upload permissions
- All dependencies available on Hackage
- Documentation builds successfully