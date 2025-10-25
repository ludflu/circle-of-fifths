.PHONY: all build clean circle keyboard diagrams help

# Default target - build and render all diagrams
all: diagrams

# Build the Haskell project
build:
	@echo "Building circle-of-fourths..."
	@cabal build

# Render the Circle of Fifths diagram
circle: build
	@echo "Generating Circle of Fifths diagram..."
	@cabal run circle-of-fourths -- -o circle.svg -w 400
	@echo "✓ Created circle.svg"

# Render the Piano keyboard with highlighted notes
keyboard: build
	@echo "Generating Piano keyboard diagram..."
	@cabal run keyboard -- -o keyboard.svg -w 600
	@echo "✓ Created keyboard.svg"

# Render all diagrams
diagrams: circle keyboard
	@echo ""
	@echo "All diagrams generated successfully!"
	@echo "  - circle.svg (Circle of Fifths)"
	@echo "  - keyboard.svg (Piano with highlighted notes)"

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	@rm -f circle.svg keyboard.svg
	@cabal clean
	@echo "✓ Cleaned"

# Display help
help:
	@echo "Available targets:"
	@echo "  make all       - Build project and generate all diagrams (default)"
	@echo "  make build     - Build the Haskell project"
	@echo "  make circle    - Generate Circle of Fifths diagram (circle.svg)"
	@echo "  make keyboard  - Generate Piano keyboard diagram (keyboard.svg)"
	@echo "  make diagrams  - Generate all diagrams"
	@echo "  make clean     - Remove generated SVG files and build artifacts"
	@echo "  make help      - Show this help message"
