#!/bin/bash

# Build script for Sliding Window Algorithms
# This script compiles all Pascal units and creates executables

set -e  # Exit on any error

echo "========================================"
echo "Building Sliding Window Algorithms"
echo "========================================"
echo ""

# Configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC_DIR="$PROJECT_ROOT/src"
BUILD_DIR="$PROJECT_ROOT/build"
BIN_DIR="$BUILD_DIR/bin"
UNITS_DIR="$BUILD_DIR/units"
REPORTS_DIR="$BUILD_DIR/reports"

# Create build directories
echo "Creating build directories..."
mkdir -p "$BIN_DIR" "$UNITS_DIR" "$REPORTS_DIR"

# Compiler settings
FPC_VERSION=$(fpc -iV 2>/dev/null || echo "Unknown")
FPC_TARGET=$(fpc -iTP 2>/dev/null || echo "Unknown")
FPC_OS=$(fpc -iTO 2>/dev/null || echo "Unknown")

echo "Compiler Information:"
echo "  Version: $FPC_VERSION"
echo "  Target: $FPC_TARGET"
echo "  OS: $FPC_OS"
echo ""

# Compiler options
FPC_OPTIONS="-Mobjfpc -Scghi -O1 -g -gl -l -vewnhibq"
UNIT_PATH="-Fu$SRC_DIR/Algorithms -Fu$SRC_DIR/Utils -Fu$SRC_DIR/Tests"
OUTPUT_PATH="-FE$BIN_DIR -FU$UNITS_DIR"

echo "Compiler Options: $FPC_OPTIONS"
echo "Unit Path: $UNIT_PATH"
echo "Output Path: $OUTPUT_PATH"
echo ""

# Function to compile a Pascal file
compile_unit() {
    local source_file="$1"
    local unit_name=$(basename "$source_file" .pas)
    
    echo "Compiling $unit_name..."
    
    if fpc $FPC_OPTIONS $UNIT_PATH $OUTPUT_PATH "$source_file"; then
        echo "  ✓ $unit_name compiled successfully"
        return 0
    else
        echo "  ✗ Failed to compile $unit_name"
        return 1
    fi
}

# Function to compile an executable
compile_executable() {
    local source_file="$1"
    local executable_name="$2"
    local output_file="$BIN_DIR/$executable_name"
    
    echo "Compiling executable $executable_name..."
    
    if fpc $FPC_OPTIONS $UNIT_PATH $OUTPUT_PATH -o"$output_file" "$source_file"; then
        echo "  ✓ $executable_name compiled successfully"
        chmod +x "$output_file" 2>/dev/null || true
        return 0
    else
        echo "  ✗ Failed to compile $executable_name"
        return 1
    fi
}

# Build process
echo "Starting build process..."
echo ""

# Step 1: Compile core algorithm units
echo "Step 1: Compiling core algorithm units"
echo "-------------------------------------"

if [ -f "$SRC_DIR/Algorithms/SlidingWindow.pas" ]; then
    compile_unit "$SRC_DIR/Algorithms/SlidingWindow.pas"
else
    echo "  ⚠ SlidingWindow.pas not found"
fi

echo ""

# Step 2: Compile utility units
echo "Step 2: Compiling utility units"
echo "-------------------------------"

if [ -f "$SRC_DIR/Utils/PerformanceAnalyzer.pas" ]; then
    compile_unit "$SRC_DIR/Utils/PerformanceAnalyzer.pas"
else
    echo "  ⚠ PerformanceAnalyzer.pas not found"
fi

echo ""

# Step 3: Compile test executable
echo "Step 3: Compiling test executable"
echo "---------------------------------"

if [ -f "$SRC_DIR/Tests/SlidingWindowTests.pas" ]; then
    compile_executable "$SRC_DIR/Tests/SlidingWindowTests.pas" "SlidingWindowTests"
else
    echo "  ⚠ SlidingWindowTests.pas not found"
fi

echo ""

# Step 4: Create benchmark executable
echo "Step 4: Creating benchmark executable"
echo "------------------------------------"

# Create benchmark program
BENCHMARK_SOURCE="$BUILD_DIR/benchmark.pas"
cat > "$BENCHMARK_SOURCE" << 'EOF'
program SlidingWindowBenchmark;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DateUtils,
  SlidingWindow, PerformanceAnalyzer;

var
  Benchmark: TSlidingWindowBenchmark;
  Report: string;
  StartTime, EndTime: TDateTime;
begin
  WriteLn('Sliding Window Algorithms Benchmark');
  WriteLn('===================================');
  WriteLn('');
  
  StartTime := Now;
  
  Benchmark := TSlidingWindowBenchmark.Create;
  try
    WriteLn('Running comprehensive benchmark suite...');
    Benchmark.RunFullBenchmarkSuite;
    
    WriteLn('Running scalability analysis...');
    Benchmark.RunScalabilityTest(1000, 10000, 2000);
    
    WriteLn('');
    WriteLn('Generating performance report...');
    Report := Benchmark.GetBenchmarkResults;
    WriteLn(Report);
    
    // Save reports
    Benchmark.SaveBenchmarkResults('./build/reports/benchmark_report.txt');
    Benchmark.Analyzer.SaveCSVReportToFile('./build/reports/benchmark_data.csv');
    
    EndTime := Now;
    WriteLn('');
    WriteLn('Benchmark completed successfully!');
    WriteLn('Total execution time: ', FormatDateTime('hh:nn:ss.zzz', EndTime - StartTime));
    WriteLn('Reports saved to ./build/reports/');
  finally
    Benchmark.Free;
  end;
end.
EOF

if compile_executable "$BENCHMARK_SOURCE" "benchmark"; then
    echo "  ✓ Benchmark executable created"
else
    echo "  ✗ Failed to create benchmark executable"
fi

echo ""

# Step 5: Build summary
echo "Build Summary"
echo "============="
echo ""

echo "Generated files:"
if [ -d "$BIN_DIR" ]; then
    ls -la "$BIN_DIR/" 2>/dev/null || echo "  No executables found"
else
    echo "  Build directory not found"
fi

echo ""
echo "Unit files:"
if [ -d "$UNITS_DIR" ]; then
    ls -la "$UNITS_DIR/"*.ppu 2>/dev/null || echo "  No unit files found"
else
    echo "  Units directory not found"
fi

echo ""
echo "Build process completed!"
echo "========================"

# Verify critical files
ERROR_COUNT=0

if [ ! -f "$BIN_DIR/SlidingWindowTests" ]; then
    echo "⚠ Warning: Test executable not found"
    ERROR_COUNT=$((ERROR_COUNT + 1))
fi

if [ ! -f "$BIN_DIR/benchmark" ]; then
    echo "⚠ Warning: Benchmark executable not found"
    ERROR_COUNT=$((ERROR_COUNT + 1))
fi

if [ $ERROR_COUNT -eq 0 ]; then
    echo "✓ All critical files built successfully"
    exit 0
else
    echo "⚠ Build completed with $ERROR_COUNT warnings"
    exit 0
fi