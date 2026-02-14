# Configuration
emacs := env("EMACS", "emacs")

# Default: run all checks
default: lint test

# Run all checks
lint: check-duplicate-defs check-imports check-cyclic-requires
    @echo "✓ All lint checks passed"

# Check for duplicate defun/defvar/defcustom/defmacro across .el files
check-duplicate-defs:
    @echo "Checking for duplicate definitions..."
    @{{ emacs }} -batch --load scripts/check-duplicate-defs.el --eval "(check-duplicate-defs)"

# Check that all modules load cleanly in batch mode
check-imports:
    @echo "Checking module imports..."
    @{{ emacs }} -batch --load scripts/check-imports.el --eval "(check-imports)"

# Check for cyclic require dependencies between .el files
check-cyclic-requires:
    @echo "Checking for cyclic require dependencies..."
    @{{ emacs }} -batch --load scripts/check-cyclic-requires.el --eval "(check-cyclic-requires)"

# Run all tests
test: test-unit
    @echo "✓ All tests passed"

# Run unit tests
test-unit:
    @echo "Running unit tests..."
    @{{ emacs }} -batch -L . -L test -l ert -l test/test-helper.el \
        -l test/claude-agent-expert-test.el \
        -l test/claude-mcp-messaging-test.el \
        -l test/claude-mcp-git-test.el \
        -l test/claude-kb-test.el \
        -f ert-run-tests-batch-and-exit

# Run a specific test file
test-file file:
    @echo "Running tests from {{ file }}..."
    @{{ emacs }} -batch -L . -L test -l ert -l test/test-helper.el -l {{ file }} -f ert-run-tests-batch-and-exit

# Run tests matching a pattern
test-pattern pattern:
    @echo "Running tests matching {{ pattern }}..."
    @{{ emacs }} -batch -L . -L test -l ert -l test/test-helper.el \
        -l test/claude-agent-expert-test.el \
        -l test/claude-mcp-messaging-test.el \
        -l test/claude-mcp-git-test.el \
        -l test/claude-kb-test.el \
        --eval "(ert-run-tests-batch-and-exit \"{{ pattern }}\")"

# Run expert system tests only (doesn't need test-helper which loads org-roam)
test-expert:
    @echo "Running expert system tests..."
    @{{ emacs }} -batch -L . -L test -l ert \
        -l test/claude-agent-expert-test.el \
        -f ert-run-tests-batch-and-exit

# Run REPL integration test framework (standalone, no external deps)
test-integration:
    @echo "Running REPL integration test framework..."
    @{{ emacs }} -batch -L . -L test -l ert \
        -l test/claude-test-framework.el \
        -l test/claude-test-dsl.el \
        -f ert-run-tests-batch-and-exit

# Run expert system integration tests (uses mock REPL framework)
test-expert-integration:
    @echo "Running expert system integration tests..."
    @{{ emacs }} -batch -L . -L test -l ert \
        -l test/claude-test-framework.el \
        -l test/claude-test-dsl.el \
        -l test/claude-expert-integration-test.el \
        -f ert-run-tests-batch-and-exit

# CI: run all checks (lint + tests)
ci: lint test
    @echo "✓ CI checks passed"
