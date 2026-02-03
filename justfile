# Configuration
emacs := env("EMACS", "emacs")

# Default: run all lint checks
default: lint

# Run all lint checks
lint: check-duplicate-defs
    @echo "✓ All lint checks passed"

# Check for duplicate defun/defvar/defcustom/defmacro across .el files
check-duplicate-defs:
    @echo "Checking for duplicate definitions..."
    @{{ emacs }} -batch --load scripts/check-duplicate-defs.el --eval "(check-duplicate-defs)"
