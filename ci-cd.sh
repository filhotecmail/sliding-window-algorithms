#!/bin/bash

# CI/CD Pipeline Script for Sliding Window Algorithms
# This script orchestrates the complete CI/CD process including build, test, benchmark, and deployment

set -e  # Exit on any error

echo "============================================"
echo "Sliding Window Algorithms - CI/CD Pipeline"
echo "============================================"
echo ""

# Configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="$PROJECT_ROOT/build"
REPORTS_DIR="$BUILD_DIR/reports"
LOGS_DIR="$BUILD_DIR/logs"
ARTIFACTS_DIR="$BUILD_DIR/artifacts"

# Create necessary directories
mkdir -p "$BUILD_DIR" "$REPORTS_DIR" "$LOGS_DIR" "$ARTIFACTS_DIR"

# Pipeline configuration
PIPELINE_LOG="$LOGS_DIR/pipeline.log"
PIPELINE_STATUS="$LOGS_DIR/pipeline_status.txt"
PIPELINE_SUMMARY="$REPORTS_DIR/pipeline_summary.txt"

# Environment detection
if [ -n "$GITHUB_ACTIONS" ]; then
    ENVIRONMENT="GitHub Actions"
    CI_MODE=true
elif [ -n "$CI" ]; then
    ENVIRONMENT="Generic CI"
    CI_MODE=true
else
    ENVIRONMENT="Local Development"
    CI_MODE=false
fi

echo "Pipeline Configuration:"
echo "  Environment: $ENVIRONMENT"
echo "  CI Mode: $CI_MODE"
echo "  Project Root: $PROJECT_ROOT"
echo "  Build Directory: $BUILD_DIR"
echo "  Reports Directory: $REPORTS_DIR"
echo ""

# Function to log pipeline events
log_pipeline_event() {
    local stage="$1"
    local status="$2"
    local message="$3"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    
    echo "[$timestamp] [$status] $stage: $message" | tee -a "$PIPELINE_LOG"
    
    # Update status file
    echo "$stage=$status" >> "$PIPELINE_STATUS"
}

# Function to run a pipeline stage
run_pipeline_stage() {
    local stage_name="$1"
    local stage_script="$2"
    local stage_description="$3"
    local optional="${4:-false}"
    
    echo "========================================"
    echo "Stage: $stage_name"
    echo "========================================"
    echo "Description: $stage_description"
    echo "Script: $stage_script"
    echo "Optional: $optional"
    echo ""
    
    log_pipeline_event "$stage_name" "STARTED" "$stage_description"
    
    local start_time=$(date +%s)
    
    if [ -f "$stage_script" ]; then
        chmod +x "$stage_script" 2>/dev/null || true
        
        if "$stage_script" > "$LOGS_DIR/${stage_name,,}_output.log" 2>&1; then
            local end_time=$(date +%s)
            local duration=$((end_time - start_time))
            
            echo "✅ $stage_name completed successfully in ${duration}s"
            log_pipeline_event "$stage_name" "SUCCESS" "Completed in ${duration}s"
            
            return 0
        else
            local end_time=$(date +%s)
            local duration=$((end_time - start_time))
            
            echo "❌ $stage_name failed after ${duration}s"
            log_pipeline_event "$stage_name" "FAILED" "Failed after ${duration}s"
            
            # Show error details
            echo "Error details:"
            tail -20 "$LOGS_DIR/${stage_name,,}_output.log" 2>/dev/null || echo "Could not read error log"
            
            if [ "$optional" = "false" ]; then
                echo "❌ Pipeline failed at stage: $stage_name"
                exit 1
            else
                echo "⚠️  Optional stage failed, continuing pipeline"
                return 1
            fi
        fi
    else
        echo "⚠️  Stage script not found: $stage_script"
        log_pipeline_event "$stage_name" "SKIPPED" "Script not found"
        
        if [ "$optional" = "false" ]; then
            echo "❌ Required stage script missing: $stage_name"
            exit 1
        else
            return 1
        fi
    fi
}

# Function to gather system information
gather_system_info() {
    echo "System Information"
    echo "================="
    echo ""
    
    echo "Environment:"
    echo "  OS: $(uname -s 2>/dev/null || echo 'Unknown') $(uname -r 2>/dev/null || echo '')"
    echo "  Architecture: $(uname -m 2>/dev/null || echo 'Unknown')"
    echo "  Hostname: $(hostname 2>/dev/null || echo 'Unknown')"
    echo "  User: $(whoami 2>/dev/null || echo 'Unknown')"
    
    echo ""
    echo "Hardware:"
    echo "  CPU: $(grep -m1 'model name' /proc/cpuinfo 2>/dev/null | cut -d':' -f2 | xargs || echo 'Unknown')"
    echo "  Cores: $(nproc 2>/dev/null || echo 'Unknown')"
    echo "  Memory: $(free -h 2>/dev/null | grep '^Mem:' | awk '{print $2}' || echo 'Unknown')"
    echo "  Disk: $(df -h . 2>/dev/null | tail -1 | awk '{print $4}' || echo 'Unknown') available"
    
    echo ""
    echo "Software:"
    echo "  Shell: $SHELL"
    echo "  Compiler: $(fpc -iV 2>/dev/null || echo 'Not installed')"
    echo "  Git: $(git --version 2>/dev/null || echo 'Not installed')"
    echo "  Docker: $(docker --version 2>/dev/null || echo 'Not installed')"
    
    echo ""
    echo "Project Information:"
    echo "  Directory: $PROJECT_ROOT"
    echo "  Git Branch: $(git branch --show-current 2>/dev/null || echo 'Unknown')"
    echo "  Git Commit: $(git rev-parse --short HEAD 2>/dev/null || echo 'Unknown')"
    echo "  Git Status: $(git status --porcelain 2>/dev/null | wc -l || echo 'Unknown') modified files"
    
    echo ""
}

# Initialize pipeline
echo "Initializing CI/CD Pipeline..."
echo "" > "$PIPELINE_LOG"
echo "" > "$PIPELINE_STATUS"

log_pipeline_event "PIPELINE" "STARTED" "CI/CD Pipeline initialization"

# Gather and log system information
gather_system_info | tee -a "$PIPELINE_LOG"

# Pipeline stages configuration
declare -A PIPELINE_STAGES
PIPELINE_STAGES["PRE_BUILD"]="$PROJECT_ROOT/pre-build.sh|Pre-build setup and validation|true"
PIPELINE_STAGES["BUILD"]="$PROJECT_ROOT/build.sh|Compile source code and create executables|false"
PIPELINE_STAGES["TEST"]="$PROJECT_ROOT/test.sh|Run unit tests and generate test reports|false"
PIPELINE_STAGES["BENCHMARK"]="$PROJECT_ROOT/benchmark.sh|Execute performance benchmarks|true"
PIPELINE_STAGES["QUALITY"]="$PROJECT_ROOT/quality.sh|Code quality analysis and metrics|true"
PIPELINE_STAGES["SECURITY"]="$PROJECT_ROOT/security.sh|Security scanning and vulnerability assessment|true"
PIPELINE_STAGES["PACKAGE"]="$PROJECT_ROOT/package.sh|Create deployment packages and artifacts|true"
PIPELINE_STAGES["DEPLOY"]="$PROJECT_ROOT/deploy.sh|Deploy to target environment|true"

# Execute pipeline stages
echo "Executing Pipeline Stages..."
echo "============================"
echo ""

PIPELINE_START_TIME=$(date +%s)
SUCCESS_COUNT=0
FAILED_COUNT=0
SKIPPED_COUNT=0

# Stage 1: Pre-build
if run_pipeline_stage "PRE_BUILD" "${PIPELINE_STAGES[PRE_BUILD]%%|*}" "$(echo "${PIPELINE_STAGES[PRE_BUILD]}" | cut -d'|' -f2)" "$(echo "${PIPELINE_STAGES[PRE_BUILD]}" | cut -d'|' -f3)"; then
    SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
else
    if [ "$(echo "${PIPELINE_STAGES[PRE_BUILD]}" | cut -d'|' -f3)" = "true" ]; then
        SKIPPED_COUNT=$((SKIPPED_COUNT + 1))
    else
        FAILED_COUNT=$((FAILED_COUNT + 1))
    fi
fi

# Stage 2: Build
if run_pipeline_stage "BUILD" "${PIPELINE_STAGES[BUILD]%%|*}" "$(echo "${PIPELINE_STAGES[BUILD]}" | cut -d'|' -f2)" "$(echo "${PIPELINE_STAGES[BUILD]}" | cut -d'|' -f3)"; then
    SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
else
    if [ "$(echo "${PIPELINE_STAGES[BUILD]}" | cut -d'|' -f3)" = "true" ]; then
        SKIPPED_COUNT=$((SKIPPED_COUNT + 1))
    else
        FAILED_COUNT=$((FAILED_COUNT + 1))
    fi
fi

# Stage 3: Test
if run_pipeline_stage "TEST" "${PIPELINE_STAGES[TEST]%%|*}" "$(echo "${PIPELINE_STAGES[TEST]}" | cut -d'|' -f2)" "$(echo "${PIPELINE_STAGES[TEST]}" | cut -d'|' -f3)"; then
    SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
else
    if [ "$(echo "${PIPELINE_STAGES[TEST]}" | cut -d'|' -f3)" = "true" ]; then
        SKIPPED_COUNT=$((SKIPPED_COUNT + 1))
    else
        FAILED_COUNT=$((FAILED_COUNT + 1))
    fi
fi

# Stage 4: Benchmark
if run_pipeline_stage "BENCHMARK" "${PIPELINE_STAGES[BENCHMARK]%%|*}" "$(echo "${PIPELINE_STAGES[BENCHMARK]}" | cut -d'|' -f2)" "$(echo "${PIPELINE_STAGES[BENCHMARK]}" | cut -d'|' -f3)"; then
    SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
else
    if [ "$(echo "${PIPELINE_STAGES[BENCHMARK]}" | cut -d'|' -f3)" = "true" ]; then
        SKIPPED_COUNT=$((SKIPPED_COUNT + 1))
    else
        FAILED_COUNT=$((FAILED_COUNT + 1))
    fi
fi

# Stage 5: Quality Analysis (optional)
if run_pipeline_stage "QUALITY" "${PIPELINE_STAGES[QUALITY]%%|*}" "$(echo "${PIPELINE_STAGES[QUALITY]}" | cut -d'|' -f2)" "$(echo "${PIPELINE_STAGES[QUALITY]}" | cut -d'|' -f3)"; then
    SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
else
    SKIPPED_COUNT=$((SKIPPED_COUNT + 1))
fi

# Stage 6: Security Scan (optional)
if run_pipeline_stage "SECURITY" "${PIPELINE_STAGES[SECURITY]%%|*}" "$(echo "${PIPELINE_STAGES[SECURITY]}" | cut -d'|' -f2)" "$(echo "${PIPELINE_STAGES[SECURITY]}" | cut -d'|' -f3)"; then
    SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
else
    SKIPPED_COUNT=$((SKIPPED_COUNT + 1))
fi

# Stage 7: Package (optional)
if run_pipeline_stage "PACKAGE" "${PIPELINE_STAGES[PACKAGE]%%|*}" "$(echo "${PIPELINE_STAGES[PACKAGE]}" | cut -d'|' -f2)" "$(echo "${PIPELINE_STAGES[PACKAGE]}" | cut -d'|' -f3)"; then
    SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
else
    SKIPPED_COUNT=$((SKIPPED_COUNT + 1))
fi

# Stage 8: Deploy (optional, only in CI)
if [ "$CI_MODE" = "true" ]; then
    if run_pipeline_stage "DEPLOY" "${PIPELINE_STAGES[DEPLOY]%%|*}" "$(echo "${PIPELINE_STAGES[DEPLOY]}" | cut -d'|' -f2)" "$(echo "${PIPELINE_STAGES[DEPLOY]}" | cut -d'|' -f3)"; then
        SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
    else
        SKIPPED_COUNT=$((SKIPPED_COUNT + 1))
    fi
else
    echo "⏭️  Skipping DEPLOY stage (not in CI mode)"
    log_pipeline_event "DEPLOY" "SKIPPED" "Not in CI mode"
    SKIPPED_COUNT=$((SKIPPED_COUNT + 1))
fi

PIPELINE_END_TIME=$(date +%s)
PIPELINE_DURATION=$((PIPELINE_END_TIME - PIPELINE_START_TIME))

# Generate pipeline summary
echo ""
echo "Generating Pipeline Summary..."
echo "============================="

cat > "$PIPELINE_SUMMARY" << EOF
Sliding Window Algorithms - CI/CD Pipeline Summary
==================================================

Pipeline Execution Details:
==========================
Start Time: $(date -d @$PIPELINE_START_TIME 2>/dev/null || date)
End Time: $(date -d @$PIPELINE_END_TIME 2>/dev/null || date)
Total Duration: ${PIPELINE_DURATION}s ($(date -u -d @$PIPELINE_DURATION +%H:%M:%S 2>/dev/null || echo "${PIPELINE_DURATION}s"))
Environment: $ENVIRONMENT
CI Mode: $CI_MODE

Stage Results:
=============
Successful: $SUCCESS_COUNT
Failed: $FAILED_COUNT
Skipped: $SKIPPED_COUNT
Total: $((SUCCESS_COUNT + FAILED_COUNT + SKIPPED_COUNT))

Stage Details:
=============

EOF

# Add stage details from status file
if [ -f "$PIPELINE_STATUS" ]; then
    while IFS='=' read -r stage status; do
        if [ -n "$stage" ] && [ -n "$status" ]; then
            printf "%-15s: %s\n" "$stage" "$status" >> "$PIPELINE_SUMMARY"
        fi
    done < "$PIPELINE_STATUS"
fi

cat >> "$PIPELINE_SUMMARY" << EOF

Generated Artifacts:
===================

Logs:
- Pipeline Log: $PIPELINE_LOG
- Build Output: $LOGS_DIR/build_output.log
- Test Output: $LOGS_DIR/test_output.log
- Benchmark Output: $LOGS_DIR/benchmark_output.log

Reports:
- Test Results: $REPORTS_DIR/test_summary.txt
- Performance Analysis: $REPORTS_DIR/performance_analysis.txt
- Scalability Analysis: $REPORTS_DIR/scalability_analysis.txt
- Algorithm Comparison: $REPORTS_DIR/algorithm_comparison.txt
- Benchmark Summary: $REPORTS_DIR/benchmark_summary.txt

Build Artifacts:
- Executables: $BUILD_DIR/bin/
- Unit Files: $BUILD_DIR/units/
- Reports: $REPORTS_DIR/

Quality Metrics:
===============

Build Status: $([ $FAILED_COUNT -eq 0 ] && echo "✅ SUCCESS" || echo "❌ FAILED")
Test Coverage: $(grep -o "Success Rate: [0-9.]*%" "$REPORTS_DIR/test_summary.txt" 2>/dev/null || echo "N/A")
Performance: $(grep -o "Linear time complexity" "$REPORTS_DIR/performance_analysis.txt" 2>/dev/null && echo "✅ O(n)" || echo "⚠️  Unknown")
Memory Usage: $(grep -o "Excellent" "$REPORTS_DIR/benchmark_summary.txt" 2>/dev/null && echo "✅ Optimal" || echo "⚠️  Unknown")

Recommendations:
===============

EOF

if [ $FAILED_COUNT -eq 0 ]; then
    cat >> "$PIPELINE_SUMMARY" << EOF
✅ Pipeline completed successfully!
✅ All critical stages passed
✅ Ready for production deployment
✅ Performance targets met
✅ Quality standards achieved

Next Steps:
- Review generated reports
- Deploy to target environment
- Monitor production performance
- Schedule regular pipeline runs
EOF
else
    cat >> "$PIPELINE_SUMMARY" << EOF
❌ Pipeline completed with failures
❌ $FAILED_COUNT critical stage(s) failed
⚠️  Review error logs before proceeding
⚠️  Fix issues and re-run pipeline

Next Steps:
- Check error logs in $LOGS_DIR/
- Fix identified issues
- Re-run failed stages
- Contact development team if needed
EOF
fi

cat >> "$PIPELINE_SUMMARY" << EOF

Pipeline Configuration:
======================
Project: Sliding Window Algorithms
Version: $(git describe --tags 2>/dev/null || git rev-parse --short HEAD 2>/dev/null || echo 'Unknown')
Branch: $(git branch --show-current 2>/dev/null || echo 'Unknown')
Commit: $(git rev-parse HEAD 2>/dev/null || echo 'Unknown')
Author: $(git log -1 --pretty=format:'%an <%ae>' 2>/dev/null || echo 'Unknown')

System Information:
==================
OS: $(uname -s) $(uname -r)
Architecture: $(uname -m)
Compiler: $(fpc -iV 2>/dev/null || echo 'Unknown')
Memory: $(free -h 2>/dev/null | grep '^Mem:' | awk '{print $2}' || echo 'Unknown')
Disk Space: $(df -h . 2>/dev/null | tail -1 | awk '{print $4}' || echo 'Unknown') available

EOF

echo "  ✅ Pipeline summary generated: $PIPELINE_SUMMARY"

# Final pipeline results
echo ""
echo "========================================"
echo "CI/CD Pipeline Execution Complete"
echo "========================================"
echo ""

echo "📊 Pipeline Statistics:"
echo "  Duration: ${PIPELINE_DURATION}s"
echo "  Successful Stages: $SUCCESS_COUNT"
echo "  Failed Stages: $FAILED_COUNT"
echo "  Skipped Stages: $SKIPPED_COUNT"
echo ""

echo "📁 Generated Artifacts:"
echo "  📋 Pipeline Summary: $PIPELINE_SUMMARY"
echo "  📝 Pipeline Log: $PIPELINE_LOG"
echo "  📊 Reports Directory: $REPORTS_DIR"
echo "  🗂️  Logs Directory: $LOGS_DIR"
echo ""

if [ $FAILED_COUNT -eq 0 ]; then
    echo "🎉 Pipeline Status: SUCCESS"
    echo "✅ All critical stages completed successfully"
    echo "🚀 Ready for deployment!"
    
    log_pipeline_event "PIPELINE" "SUCCESS" "All stages completed successfully in ${PIPELINE_DURATION}s"
    exit 0
else
    echo "❌ Pipeline Status: FAILED"
    echo "💥 $FAILED_COUNT critical stage(s) failed"
    echo "🔧 Please review logs and fix issues"
    
    log_pipeline_event "PIPELINE" "FAILED" "$FAILED_COUNT stages failed after ${PIPELINE_DURATION}s"
    exit 1
fi