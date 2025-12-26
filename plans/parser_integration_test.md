# Parser Integration Tests - Phase 19

## Overview
Integration testing for the parser phase has been implemented following the pattern established by the lexer integration tests.

## Implementation Details

### Test Harness Location
- **Source**: `src/parser.rs` (within `#[cfg(test)]` module)
- **Test Function**: `parse_file_tests()`

### Test Files
Parser integration tests read `.manta` source files and generate expected AST output as `.json` files.

**Location**: `tests/parser/`

### Test Structure
1. Read all `.manta` files from `tests/parser/`
2. Lex and parse each file
3. Serialize the AST to JSON
4. Compare against expected `.json` output
5. If `.json` doesn't exist, generate it (and fail test to prompt verification)

### Passing Tests (3/4)
The following test files successfully parse and have verified JSON output:
- `defer_free.manta` → `defer_free.json` ✓
- `option_match.manta` → `option_match.json` ✓
- `try_catch.manta` → `try_catch.json` ✓

### Known Issue: nil_refs.manta
**Status**: Parser error during parsing
**Issue**: Dereference with assignment (`*p = 42`) triggers parsing error
**Error**: `UnexpectedToken("No prefix parselet for token kind: Equal")`
**Root Cause**: To be debugged - likely issue in how expression parsing terminates when encountering assignment operator
**Impact**: Blocks generation of `nil_refs.json`

## AST Serialization
All AST types have been updated with `#[derive(Serialize, Deserialize)]`:
- `src/ast.rs`: All structs and enums now support JSON serialization
- Dependencies: `serde` and `serde_json` already in `Cargo.toml`

## Test Execution
Run parser integration tests:
```bash
cargo test parse_file_tests
```

Current status: 138 unit tests pass, 1 integration test fails (nil_refs parsing error)

## Future Work
1. Debug and fix the nil_refs parsing issue
2. Add nil_refs.json to passing tests
3. Consider adding more complex test cases
4. Extend integration tests to cover semantic validation (future phase)
