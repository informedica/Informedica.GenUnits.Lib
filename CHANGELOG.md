# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.2] - 2021-01-20

### Changed
- moved lib to informedica
- use new infrastructure
- target net5 only

## [1.0.1-beta]

* Fixed a bug with simplifying combined units

## [1.0.0-beta]

* Complete workover of the core code
* Moved everything to dotnetcore

## [0.4.5-beta]

* Updated GenUtils, fixed missing assemblyinfo

## [0.4.4-beta]

* UnitGroup getUnits also gets a unit for an arbitrary general unit group
* Updated setup to build the project on GitHub

## [0.4.3-beta]

* Fixed bug in molar group
* Added test to cover the bug

## [0.4.2-beta]

* Fixed bug in find unit with group
* Added test to cover the bug

## [0.4.1-beta]

* Use brackets to delimit unit groups instead of parentheses
* Update docs

## [0.4.0-beta]

* Added possibility to calculate with unit groups
* Convert a unit group to all possible units
* Removed general group, instead create special group with multiplier 1 if not an existing unit
* Changed toString and fromString to specify group name

## [0.3.1-beta]

* Added tests
* Removed dependencies on FsCheck and Unquote
* Added utils lib dependency

## [0.3.0-beta]

* Added specific Api
* Added simple eval to evaluate expressions with value units
* Updated the tutorial

## [0.2.0-beta]

* Added fromString to create a value unit from a string
* Updated the tutorial

## [0.1.1-beta]

* First working version
* Calcultate with units
* Convert to readable strings

## [0.0.1-beta]

* Initial setup
[Unreleased]: https://github.com/informedica/Informedica.GenUnits.Lib/compare/v1.0.2...HEAD
[1.0.2]: https://github.com/informedica/Informedica.GenUnits.Lib/compare/v1.0.1-beta...v1.0.2
