# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Fixed
- Fix issue with lines containing leading minus signs "-". (Thanks to
  medranocalvo!)
- Fix issue with lines containing trailing semicolon ";".
### Changed
- License language
### Deprecated
- Possibility to configure terminal using org-babel header arguments.
### Added
- Add installation instructions for MELPA.
- Add possibility to configure terminal and add options to the
  terminal. (Thanks to dkrm0!)

## [0.1.5] - 2018-07-19
### Changed
- Make variables defcustom
- Rename internal functions

## [0.1.1] - 2018-07-17
### Added
- Initial release

[Unreleased]: https://github.com/ahendriksen/ob-tmux/compare/0.1.5...HEAD
[0.1.5]: https://github.com/ahendriksen/ob-tmux/compare/0.1.1...0.1.5
[0.1.1]: https://github.com/ahendriksen/ob-tmux/tree/0.1.1
