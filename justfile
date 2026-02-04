# Configuration
emacs := env("EMACS", "emacs")

# Default: run all checks
default: lint

# Run all checks
lint: check-duplicate-defs check-imports
    @echo "✓ All checks passed"

# Check for duplicate defun/defvar/defcustom/defmacro across .el files
check-duplicate-defs:
    @echo "Checking for duplicate definitions..."
    @{{ emacs }} -batch --load scripts/check-duplicate-defs.el --eval "(check-duplicate-defs)"

# Check that all modules load cleanly in batch mode
check-imports:
    @echo "Checking module imports..."
    @{{ emacs }} -batch --load scripts/check-imports.el --eval "(check-imports)"
