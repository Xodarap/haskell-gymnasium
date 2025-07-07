# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.0.0] - 2025-07-07

### Added
- Initial release of gym-hs
- Basic Gymnasium environment bindings
- Type-safe API for actions and observations
- Subprocess-based Python interoperability
- Support for environment creation, reset, step, and render operations
- Comprehensive error handling with GymError types
- Automatic resource management and cleanup
- JSON-based communication protocol
- Support for all standard Gymnasium environments (CartPole, MountainCar, etc.)
- Complete test suite with hspec
- Example application demonstrating usage
- Documentation and README

### Core Features
- `makeEnv` - Create Gymnasium environments
- `reset` - Reset environments to initial state
- `step` - Take actions in environments
- `render` - Render environment visualization
- `closeEnv` - Clean up environment resources

### Dependencies
- base >= 4.7 && < 5
- process >= 1.6
- aeson >= 2.0
- bytestring >= 0.10
- text >= 1.2
- vector >= 0.12
- containers >= 0.6
- unordered-containers >= 0.2
- scientific >= 0.3

### Requirements
- GHC 9.2+
- Python 3.8+
- Gymnasium Python library (`pip install gymnasium`)

[Unreleased]: https://github.com/gym-hs/gym-hs/compare/v0.1.0.0...HEAD
[0.1.0.0]: https://github.com/gym-hs/gym-hs/releases/tag/v0.1.0.0