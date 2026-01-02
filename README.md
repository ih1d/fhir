# Haskell FHIR Implementation — Weekly Task Breakdown

## Package Priority

### Active Development (2026)

| Package | Purpose | Phase |
| --------- | --------- | ------- |
| `fhir-types` | Core FHIR R4 data types | 1 |
| `fhir-json` | JSON serialization | 2 |
| `fhir-xml` | XML serialization | 2 |
| `fhir-client` | Connect TO other FHIR servers | 2 |
| `fhir-validation` | Validate against profiles | 3 |

### Deferred (2027 — Integration Platform)

| Package | Purpose | Why Deferred |
| --------- | --------- | -------------- |
| `fhir-server` | BE a FHIR server | Complex; not needed for Validation Service |
| `fhir-smart` | SMART on FHIR auth | Only needed for server |

## Phase 1: Foundation & Core Types (Jan 6 – Feb 16)

### Week 1 (Jan 6-12): Project Infrastructure

- [X] Create GitHub repository with MIT/Apache-2.0 dual license
- [X] Initialize multi-package Cabal project structure:

```sh
  fhir/
  ├── cabal.project
  ├── fhir-types/
  ├── fhir-json/
  ├── fhir-xml/
  ├── fhir-validation/
  ├── fhir-client/
  └── fhir-server/
```

- [X] Configure GHC 9.6+ with recommended extensions
- [ ] Set up GitHub Actions CI (build, test, hlint, ormolu)
- [ ] Set up pre-commit hooks (hlint, ormolu)

### Week 2 (Jan 13-19): FHIR Specification Analysis

- [ ] Download FHIR R5 specification bundle from hl7.org
- [ ] Download all StructureDefinitions (JSON format)
- [ ] Analyze JSON schema for type generation strategy
- [ ] Document choice type patterns (value[x])
- [ ] Document extension patterns (_field)
- [ ] Create design doc for type generation approach
- [ ] Decision: Template Haskell vs external code generator

### Week 3 (Jan 20-26): Primitive Types

- [ ] Implement core primitive types module:

```haskell
  -- fhir-types/src/FHIR/Types/Primitives.hs
  newtype Id = Id Text
  newtype Uri = Uri Text
  newtype Canonical = Canonical Text
  newtype Code = Code Text
  newtype Oid = Oid Text
  newtype Uuid = Uuid Text
  newtype Markdown = Markdown Text
  newtype Base64Binary = Base64Binary ByteString
  newtype Instant = Instant UTCTime
  data FhirDate = YearOnly Year | YearMonth Year Month | FullDate Day
  data FhirDateTime = ...
  newtype FhirTime = FhirTime TimeOfDay
  newtype Decimal = Decimal Scientific
  newtype Integer' = Integer' Integer
  newtype PositiveInt = PositiveInt Natural
  newtype UnsignedInt = UnsignedInt Natural
```

- [ ] Implement smart constructors with validation
- [ ] Write QuickCheck Arbitrary instances
- [ ] Write property tests for all primitives

### Week 4 (Jan 27 - Feb 2): Complex Data Types

- [ ] Implement Element base type with extensions
- [ ] Implement core complex types:
  - Identifier, HumanName, Address, ContactPoint
  - CodeableConcept, Coding, Quantity, Range, Ratio
  - Period, Timing, Attachment, Reference
  - Narrative, Meta, Extension
- [ ] Handle polymorphic references (Reference to multiple resource types)
- [ ] Write tests with FHIR example data

### Week 5 (Feb 3-9): Type Generator - Part 1

- [ ] Create StructureDefinition parser
- [ ] Build type generator targeting Tier 1 resources:
  - Patient, Practitioner, Organization
  - Observation, Encounter, Condition
  - Medication, Bundle
- [ ] Generate Haskell types from StructureDefinitions
- [ ] Handle required vs optional fields (Maybe)
- [ ] Handle cardinality (single value vs list)

### Week 6 (Feb 10-16): Type Generator - Part 2

- [ ] Add Tier 2 resources to generator:
  - MedicationRequest, DiagnosticReport, Procedure
  - Immunization, AllergyIntolerance, CarePlan
  - DocumentReference, Location
- [ ] Implement choice types (value[x]) as sum types
- [ ] Generate contained resources handling
- [ ] Create fhir-types package documentation
- [ ] **MILESTONE: Publish fhir-types 0.1.0 to Hackage (alpha)**

---

## Phase 2: Serialization & Client (Feb 17 – Mar 30)

### Week 7 (Feb 17-23): JSON Encoding

- [ ] Create custom Aeson ToJSON instances
- [ ] Handle resourceType field injection
- [ ] Implement choice type encoding (valueString, valueQuantity, etc.)
- [ ] Handle null vs absent vs empty array distinctions
- [ ] Test against 10 official FHIR Patient examples

### Week 8 (Feb 24 - Mar 2): JSON Decoding

- [ ] Create custom Aeson FromJSON instances
- [ ] Handle primitive extensions (_birthDate pattern)
- [ ] Implement error messages with JSON path context
- [ ] Handle contained resources deserialization
- [ ] Round-trip test all Tier 1 resources against official examples

### Week 9 (Mar 3-9): XML Support

- [ ] Create XML serialization using xml-conduit
- [ ] Handle [FHIR namespace](http://hl7.org/fhir)
- [ ] Implement element ordering per schema
- [ ] Handle Narrative (embedded XHTML)
- [ ] Support both compact and pretty-printed output

### Week 10 (Mar 10-16): XML Streaming & Completion

- [ ] Implement streaming XML for large Bundles
- [ ] Round-trip test XML serialization
- [ ] Benchmark JSON vs XML performance
- [ ] **MILESTONE: Publish fhir-json 0.1.0 and fhir-xml 0.1.0**

### Week 11 (Mar 17-23): FHIR Client Core

- [ ] Design type-safe API for FHIR operations:

```haskell
  read :: FhirClient -> ResourceType r -> Id -> IO (Either FhirError (Resource r))
  create :: FhirClient -> Resource r -> IO (Either FhirError (Resource r))
  update :: FhirClient -> Resource r -> IO (Either FhirError (Resource r))
  delete :: FhirClient -> ResourceType r -> Id -> IO (Either FhirError ())
```

- [ ] Implement connection configuration (base URL, auth)
- [ ] Handle FHIR-specific HTTP headers
- [ ] Implement error handling (OperationOutcome parsing)

### Week 12 (Mar 24-30): FHIR Client Search

- [ ] Design type-safe search parameters:

```haskell
  search :: FhirClient -> SearchParams r -> IO (Either FhirError (Bundle r))
```

- [ ] Implement common search parameters ``_id, _lastUpdated``
- [ ] Handle search modifiers (:exact, :contains, :missing)
- [ ] Implement ``_include, _revinclude``
- [ ] Handle pagination (next, previous links)
- [ ] Test against HAPI FHIR public server
- [ ] **MILESTONE: Publish fhir-client 0.1.0**

---

## Phase 3: Validation & Profiles (Mar 31 – Apr 27)

### Week 13 (Mar 31 - Apr 6): Validation Framework

- [ ] Design validation result type:

```haskell
  data ValidationResult 
    = Valid
    | Invalid [ValidationError]
  
  data ValidationError = ValidationError
    { errorPath :: FhirPath
    , errorCode :: ErrorCode
    , errorMessage :: Text
    , errorSeverity :: Severity
    }
```

- [ ] Create StructureDefinition evaluator
- [ ] Implement cardinality validation (min/max)
- [ ] Implement fixed value and pattern validation

### Week 14 (Apr 7-13): FHIRPath Subset

- [ ] Implement FHIRPath lexer and parser
- [ ] Support essential operations:
  - Navigation: `.`, `[]`, `children()`
  - Existence: `exists()`, `empty()`, `count()`
  - Comparison: `=`, `!=`, `<`, `>`, `<=`, `>=`
  - Boolean: `and`, `or`, `not`, `implies`
  - Type: `is`, `as`, `ofType()`
- [ ] Implement invariant evaluation
- [ ] Test with FHIR base resource invariants

### Week 15 (Apr 14-20): US Core Profiles

- [ ] Download US Core 6.1 StructureDefinitions
- [ ] Generate type-safe profile wrappers:

```haskell
newtype USCorePatient = USCorePatient { unUSCorePatient :: Patient }
  
  toUSCorePatient :: Patient -> ValidationResult USCorePatient
```

- [ ] Implement must-support validation
- [ ] Add US Core specific constraints

### Week 16 (Apr 21-27): Terminology Basics

- [ ] Implement ValueSet validation (basic)
- [ ] Support required, extensible, preferred bindings
- [ ] Add code system validation for common systems:
  - LOINC, SNOMED CT, ICD-10, RxNorm
- [ ] Test against US Core required ValueSets
- [ ] **MILESTONE: Publish fhir-validation 0.1.0**

---

## Phase 4: MVP & Demo (Apr 28 – May 25)

### Week 17 (Apr 28 - May 4): Demo Application Design

- [ ] Design demo app: "FHIR Data Aggregator"
  - Connects to multiple FHIR servers
  - Fetches and validates patient data
  - Demonstrates type-safety benefits
- [ ] Set up demo project structure
- [ ] Implement server connection management

### Week 18 (May 5-11): Demo Application Development

- [ ] Build patient search and retrieval
- [ ] Implement data aggregation across servers
- [ ] Add validation reporting with clear error display
- [ ] Create CLI interface for demo
- [ ] Add example configurations for public FHIR servers

### Week 19 (May 12-18): Documentation

- [ ] Write "Getting Started" tutorial
- [ ] Create API documentation (Haddock)
- [ ] Build cookbook with examples:
  - Reading patient data
  - Creating observations
  - Searching with parameters
  - Validating against US Core
- [ ] Set up documentation website (GitHub Pages or Read the Docs)

### Week 20 (May 19-25): Polish & Release

- [ ] Performance benchmarking vs HAPI FHIR
- [ ] Security review (no sensitive data in errors)
- [ ] Record demo video (5-10 minutes)
- [ ] Create one-page marketing PDF
- [ ] Final testing across all packages
- [ ] **MILESTONE: Publish all packages to Hackage (beta)**
- [ ] **MILESTONE: Working prototype complete**

---

## Phase 5: Customer Discovery (May 26 – Aug 31)

### Monthly Goals

**June:**

- [ ] Identify 30 target contacts (health tech CTOs, developers)
- [ ] Conduct 8-10 customer discovery interviews
- [ ] Present at 1-2 Haskell meetups (virtual or in-person)
- [ ] Gather feedback on pain points and feature priorities
- [ ] Begin iterating based on feedback

**July:**

- [ ] Conduct 8-10 more interviews
- [ ] Identify 3-5 potential pilot customers
- [ ] Start pilot program (free access for feedback)
- [ ] Submit talk proposal to health tech conference
- [ ] Begin SMART on FHIR development if customer demand

**August:**

- [ ] Continue pilot program
- [ ] Collect testimonials from happy pilots
- [ ] Iterate on major friction points
- [ ] Finalize pricing strategy
- [ ] Prepare for paid launch

---

## Phase 6: Production & Revenue (Sep 1 – Dec 31)

### September

- [ ] Launch paid tier
- [ ] Convert 1-2 pilots to paid
- [ ] Set up billing (Stripe)
- [ ] Create terms of service and privacy policy

### October

- [ ] Expand marketing (blog posts, case studies)
- [ ] Target additional customers
- [ ] Consider FHIR server development based on demand
- [ ] Explore consulting/training revenue

### November

- [ ] Continue customer acquisition
- [ ] Plan 2027 roadmap based on customer needs
- [ ] Evaluate hiring needs

### December

- [ ] Year-end review
- [ ] Update all packages to 1.0.0 if stable
- [ ] **MILESTONE: 3+ paying customers, $5K+ MRR**

---

## Quick Reference: Key Technical Decisions

| Decision | Recommendation | Rationale |
| ---------- | ---------------- | ----------- |
| GHC Version | 9.6+ | Modern features, good performance |
| Type generation | External generator | Faster iteration, easier debugging |
| JSON library | Aeson (custom) | Industry standard, FHIR needs custom handling |
| XML library | xml-conduit | Streaming, good perf |
| Web framework | Servant | Type-safe APIs |
| Database | PostgreSQL + JSONB | Schema flexibility for FHIR |
| Testing | Hspec + QuickCheck | Standard, property testing |
| License | MIT or Apache-2.0 | Business-friendly |

---

## Resources & Links

- [FHIR R4 Spec](https://hl7.org/fhir/R4/)
- [US Core 6.1](https://hl7.org/fhir/us/core/)
- [SMART on FHIR](https://docs.smarthealthit.org/)
- [HAPI Test Server](https://hapi.fhir.org/)
- [FHIR Examples](https://hl7.org/fhir/R4/downloads.html)
