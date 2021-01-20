# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed
- moved lib to informedica
- use new infrastructure
- target net5 only


## 1.0.1-beta - Dec 3 2019
* Fixed a bug with simplifying combined units

## 1.0.0-beta - Dec 3 2019
* Complete workover of the core code
* Moved everything to dotnetcore

## 0.4.5-beta - June 06 2016
* Updated GenUtils, fixed missing assemblyinfo

## 0.4.4-beta - May 28 2016
* UnitGroup getUnits also gets a unit for an arbitrary general unit group
* Updated setup to build the project on GitHub

## 0.4.3-beta - May 21 2016
* Fixed bug in molar group
* Added test to cover the bug

## 0.4.2-beta - May 20 2016
* Fixed bug in find unit with group
* Added test to cover the bug

## 0.4.1-beta - May 20 2016
* Use brackets to delimit unit groups instead of parentheses
* Update docs

## 0.4.0-beta - May 19 2016
* Added possibility to calculate with unit groups
* Convert a unit group to all possible units
* Removed general group, instead create special group with multiplier 1 if not an existing unit
* Changed toString and fromString to specify group name

## 0.3.1-beta - May 15 2016
* Added tests
* Removed dependencies on FsCheck and Unquote
* Added utils lib dependency

## 0.3.0-beta - May 13 2016
* Added specific Api
* Added simple eval to evaluate expressions with value units
* Updated the tutorial

## 0.2.0-beta - May 12 2016
* Added fromString to create a value unit from a string
* Updated the tutorial

## 0.1.1-beta - May 11 2016
* First working version
* Calcultate with units
* Convert to readable strings

## 0.0.1-beta - April 29 2016
* Initial setup


