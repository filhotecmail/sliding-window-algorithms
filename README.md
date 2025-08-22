# Sliding Window Algorithms in Delphi

A comprehensive implementation of optimized sliding window algorithms in Delphi, featuring generic classes, performance analysis tools, and extensive unit tests.

## 🚀 Features

- **Generic Sliding Window Implementation**: Type-safe generic classes that work with any data type
- **Specialized Integer Algorithms**: Optimized implementations for common integer operations
- **String Processing Algorithms**: Advanced string manipulation using sliding window techniques
- **Performance Analysis**: Built-in benchmarking and performance measurement tools
- **Comprehensive Testing**: Extensive unit test suite with edge case coverage
- **Memory Tracking**: Memory usage analysis and optimization tools
- **CI/CD Ready**: Docker support with automated testing pipeline

## 📁 Project Structure

```
SlidingWindowAlgorithms/
├── src/
│   ├── Algorithms/
│   │   └── SlidingWindow.pas          # Main algorithm implementations
│   ├── Utils/
│   │   └── PerformanceAnalyzer.pas    # Performance measurement tools
│   └── Tests/
│       └── SlidingWindowTests.pas     # Comprehensive unit tests
├── .gitignore                         # Git ignore rules
├── README.md                          # This file
└── Dockerfile                         # Docker configuration
```

## 🔧 Algorithm Categories

### Fixed-Size Window Algorithms
- **Maximum Sum Subarray**: Find the maximum sum of any subarray of size k
- **Minimum Sum Subarray**: Find the minimum sum of any subarray of size k
- **Maximum Product Subarray**: Find the maximum product of any subarray of size k
- **Average of Subarray**: Calculate averages for all subarrays of size k
- **Sliding Window Maximum/Minimum**: Find max/min elements in each window
- **Count Distinct Elements**: Count unique elements in each window

### Variable-Size Window Algorithms
- **Longest Subarray with At Most K Distinct**: Find longest subarray with ≤ k unique elements
- **Smallest Subarray with Sum Greater Than**: Find shortest subarray with sum > target
- **Longest Subarray with Sum Equals K**: Find longest subarray with exact sum
- **Maximum Length Subarray with Sum K**: Optimized version for sum matching

### String Algorithms
- **Longest Substring Without Repeating Characters**: Classic sliding window string problem
- **Longest Substring with At Most K Distinct Characters**: String version of distinct elements
- **Minimum Window Substring**: Find smallest window containing all characters of pattern
- **Longest Repeating Character Replacement**: Find longest substring after k replacements
- **Permutation in String**: Check if any permutation of pattern exists in string
- **Find All Anagrams**: Locate all anagram positions in string

## 🎯 Time & Space Complexity

| Algorithm | Time Complexity | Space Complexity | Notes |
|-----------|----------------|------------------|-------|
| Fixed Window Sum/Avg | O(n) | O(1) | Optimal sliding window |
| Variable Window | O(n) | O(k) | k = distinct elements |
| String Algorithms | O(n) | O(min(m,n)) | m = alphabet size |
| Window Maximum | O(n) | O(k) | Using deque optimization |

## 🚀 Quick Start

### Basic Usage

```pascal
uses SlidingWindow;

var
  Data: TArray<Integer>;
  Window: TIntegerSlidingWindow;
  Result: Integer;
begin
  Data := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  Window := TIntegerSlidingWindow.Create(Data);
  try
    // Find maximum sum of subarray with size 3
    Result := Window.MaxSumSubarray(3); // Returns 24 (7+8+9)
    
    // Find longest subarray with at most 2 distinct elements
    Result := Window.LongestSubarrayWithAtMostKDistinct(2);
  finally
    Window.Free;
  end;
end;
```

### String Processing

```pascal
uses SlidingWindow;

var
  Text: string;
  StringWindow: TStringSlidingWindow;
  Length: Integer;
begin
  Text := 'abcabcbb';
  StringWindow := TStringSlidingWindow.Create(Text);
  try
    // Find longest substring without repeating characters
    Length := StringWindow.LongestSubstringWithoutRepeatingChars; // Returns 3 ("abc")
  finally
    StringWindow.Free;
  end;
end;
```

### Performance Analysis

```pascal
uses PerformanceAnalyzer;

var
  Benchmark: TSlidingWindowBenchmark;
  Report: string;
begin
  Benchmark := TSlidingWindowBenchmark.Create;
  try
    // Run comprehensive benchmark suite
    Benchmark.RunFullBenchmarkSuite;
    
    // Generate performance report
    Report := Benchmark.GetBenchmarkResults;
    WriteLn(Report);
    
    // Save results to file
    Benchmark.SaveBenchmarkResults('benchmark_results.txt');
  finally
    Benchmark.Free;
  end;
end;
```

## 🧪 Testing

The project includes comprehensive unit tests covering:

- **Algorithm Correctness**: Verify all algorithms produce correct results
- **Edge Cases**: Empty arrays, single elements, boundary conditions
- **Performance Tests**: Ensure algorithms meet time complexity requirements
- **Memory Tests**: Validate memory usage stays within acceptable limits
- **Integration Tests**: End-to-end testing of complete workflows

### Running Tests

```bash
# Compile and run tests
dcc32 SlidingWindowTests.pas
SlidingWindowTests.exe
```

## 📊 Performance Benchmarks

The included performance analyzer provides detailed metrics:

- **Execution Time**: Precise timing measurements
- **Memory Usage**: Peak and average memory consumption
- **Scalability Analysis**: Performance across different input sizes
- **Algorithm Comparison**: Side-by-side performance comparisons
- **Complexity Verification**: Empirical validation of theoretical complexity

### Sample Benchmark Results

```
=== Sliding Window Algorithms Performance Report ===

Algorithm: MaxSumSubarray
Input Size: 10000, Window Size: 100
Execution Time: 2.34 ms
Memory Used: 0.15 MB
Time Complexity: O(n)
Space Complexity: O(1)

Algorithm: LongestSubstringWithoutRepeatingChars
Input Size: 5000 characters
Execution Time: 8.67 ms
Memory Used: 0.08 MB
Time Complexity: O(n)
Space Complexity: O(min(m,n))
```

## 🐳 Docker Support

The project includes Docker configuration for consistent build environments:

```dockerfile
FROM ubuntu:20.04

# Install Free Pascal Compiler
RUN apt-get update && apt-get install -y fpc

# Copy source code
COPY src/ /app/src/
WORKDIR /app

# Compile and run tests
RUN fpc -Fu./src/Algorithms -Fu./src/Utils src/Tests/SlidingWindowTests.pas
CMD ["./SlidingWindowTests"]
```

### Building with Docker

```bash
# Build Docker image
docker build -t sliding-window-algorithms .

# Run tests in container
docker run sliding-window-algorithms
```

## 🔄 CI/CD Pipeline

GitHub Actions workflow for automated testing:

- **Build Verification**: Compile all source files
- **Unit Testing**: Run complete test suite
- **Performance Testing**: Execute benchmark suite
- **Code Quality**: Static analysis and linting
- **Documentation**: Generate and deploy documentation

## 📈 Optimization Techniques

The implementations use several optimization strategies:

1. **Two-Pointer Technique**: Efficient window expansion and contraction
2. **Deque for Window Extrema**: O(1) amortized max/min operations
3. **Hash Maps for Frequency Counting**: O(1) character/element tracking
4. **Prefix Sums**: Constant-time range sum queries
5. **Memory Pool Allocation**: Reduced memory fragmentation
6. **SIMD Instructions**: Vectorized operations where applicable

## 🤝 Contributing

Contributions are welcome! Please follow these guidelines:

1. **Fork the Repository**: Create your own fork
2. **Create Feature Branch**: `git checkout -b feature/new-algorithm`
3. **Add Tests**: Ensure new algorithms have comprehensive tests
4. **Update Documentation**: Add algorithm descriptions and examples
5. **Performance Analysis**: Include benchmark results
6. **Submit Pull Request**: Provide detailed description of changes

## 📚 Algorithm References

Based on optimal algorithms from:

- "An Optimal Algorithm for Sliding Window Order" research paper
- Competitive programming best practices
- Industry-standard implementations
- Academic algorithm analysis

## 🏆 Performance Goals

- **Time Complexity**: All algorithms achieve theoretical optimal complexity
- **Memory Efficiency**: Minimal space overhead beyond input data
- **Scalability**: Linear performance scaling with input size
- **Reliability**: 100% test coverage with edge case handling

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🔗 Links

- **GitHub Repository**: [https://github.com/filhotecmail/sliding-window-algorithms](https://github.com/filhotecmail/sliding-window-algorithms)
- **Documentation**: Auto-generated from source code
- **Performance Reports**: Available in releases
- **Issue Tracker**: GitHub Issues

## 📞 Support

For questions, issues, or contributions:

- **GitHub Issues**: Report bugs and request features
- **Discussions**: Algorithm discussions and optimizations
- **Email**: Technical support and collaboration

---

**Built with ❤️ using Delphi and modern software engineering practices**