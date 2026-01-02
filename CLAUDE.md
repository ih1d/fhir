# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is `fhir-validation`, one package in a Haskell multi-package workspace for FHIR (Fast Healthcare Interoperability Resources) implementation. The workspace creates a type-safe FHIR R4/R5 library suite.

## Build Commands

```bash
# Build all packages in workspace
cabal build

# Build specific package
cabal build fhir-validation

# Run all tests
cabal test

# Run specific package tests
cabal test fhir-validation

# Check build without compiling
cabal build --dry-run
```

## Workspace Architecture

The project uses a multi-package Cabal workspace (see `../cabal.project`):

```
fhir/
├── cabal.project          # Workspace config (GHC 9.6+)
├── fhir-types/            # Core FHIR R4 data types (Phase 1)
├── fhir-json/             # Aeson-based JSON serialization (Phase 2)
├── fhir-xml/              # xml-conduit XML serialization (Phase 2)
├── fhir-client/           # Type-safe FHIR server client (Phase 2)
└── fhir-validation/       # Validation against StructureDefinitions (Phase 3)
```

**Dependency hierarchy**: fhir-types → fhir-json/fhir-xml → fhir-validation → fhir-client

## Key Technical Decisions

- **GHC 9.6+** with strict warnings (`-Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints`)
- **External code generator** for FHIR types from StructureDefinitions (not Template Haskell)
- **Aeson** for JSON with custom instances for FHIR-specific patterns
- **xml-conduit** for XML serialization
- **Hspec + QuickCheck** for testing

## FHIR-Specific Patterns

When implementing FHIR types, handle these patterns:

1. **Choice types (value[x])**: Polymorphic fields encoded as sum types (e.g., `valueString`, `valueQuantity`)
2. **Primitive extensions (_field)**: FHIR primitives can have extensions via underscore-prefixed sibling fields
3. **resourceType field**: Must be injected in JSON output
4. **Contained resources**: Resources nested within other resources
5. **Partial dates**: FhirDate supports YearOnly, YearMonth, or FullDate

## Validation Package Scope (fhir-validation)

This package will implement:
- StructureDefinition evaluation
- Cardinality validation (min/max)
- FHIRPath expression subset for invariants
- ValueSet binding validation
- US Core profile support
