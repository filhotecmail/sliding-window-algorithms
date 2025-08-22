unit SlidingWindowTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, SlidingWindow, PerformanceAnalyzer;

type
  // Test case for Integer Sliding Window algorithms
  TIntegerSlidingWindowTest = class(TTestCase)
  private
    FSlidingWindow: TIntegerSlidingWindow;
    FTestData: TArray<Integer>;
    FPerformanceAnalyzer: TPerformanceAnalyzer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Fixed-size window tests
    procedure TestMaxSumSubarray;
    procedure TestMinSumSubarray;
    procedure TestMaxProductSubarray;
    procedure TestAverageOfSubarray;
    procedure TestSlidingWindowMaximum;
    procedure TestSlidingWindowMinimum;
    procedure TestCountDistinctInWindow;
    
    // Variable-size window tests
    procedure TestLongestSubarrayWithAtMostKDistinct;
    procedure TestSmallestSubarrayWithSumGreaterThan;
    procedure TestLongestSubarrayWithSumEqualsK;
    procedure TestMaxLengthSubarrayWithSumK;
    procedure TestKthSmallestInSlidingWindow;
    procedure TestKthSmallestAtPosition;
    
    // Edge cases
    procedure TestEmptyArray;
    procedure TestSingleElement;
    procedure TestWindowSizeGreaterThanArray;
    procedure TestNegativeNumbers;
    procedure TestAllSameElements;
    
    // Performance tests
    procedure TestPerformanceWithLargeData;
    procedure TestMemoryUsage;
  end;

  // Test case for String Sliding Window algorithms
  TStringSlidingWindowTest = class(TTestCase)
  private
    FStringSlidingWindow: TStringSlidingWindow;
    FTestString: string;
    FPerformanceAnalyzer: TPerformanceAnalyzer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // String algorithm tests
    procedure TestLongestSubstringWithoutRepeatingChars;
    procedure TestLongestSubstringWithAtMostKDistinctChars;
    procedure TestMinimumWindowSubstring;
    procedure TestLongestRepeatingCharacterReplacement;
    procedure TestPermutationInString;
    procedure TestFindAllAnagrams;
    
    // Edge cases
    procedure TestEmptyString;
    procedure TestSingleCharacter;
    procedure TestAllSameCharacters;
    procedure TestNoValidWindow;
    
    // Performance tests
    procedure TestStringPerformanceWithLargeData;
  end;

  // Test case for Generic Sliding Window
  TGenericSlidingWindowTest = class(TTestCase)
  private
    FGenericWindow: TSlidingWindow<Double>;
    FDoubleData: TArray<Double>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenericMaxSum;
    procedure TestGenericMinSum;
    procedure TestGenericAverage;
    procedure TestGenericCustomFunction;
  end;

  // Integration test suite
  TIntegrationTestSuite = class(TTestCase)
  private
    FBenchmark: TSlidingWindowBenchmark;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFullBenchmarkSuite;
    procedure TestScalabilityTest;
    procedure TestReportGeneration;
    procedure TestCSVReportGeneration;
  end;

implementation

uses
  Math;

{ TIntegerSlidingWindowTest }

procedure TIntegerSlidingWindowTest.SetUp;
begin
  inherited SetUp;
  FTestData := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  FSlidingWindow := TIntegerSlidingWindow.Create(FTestData);
  FPerformanceAnalyzer := TPerformanceAnalyzer.Create;
end;

procedure TIntegerSlidingWindowTest.TearDown;
begin
  FSlidingWindow.Free;
  FPerformanceAnalyzer.Free;
  inherited TearDown;
end;

procedure TIntegerSlidingWindowTest.TestMaxSumSubarray;
var
  Result: Integer;
begin
  // Test with window size 3
  Result := FSlidingWindow.MaxSumSubarray(3);
  CheckEquals(24, Result, 'Max sum of subarray with window size 3 should be 24 (7+8+9)');
  
  // Test with window size 1
  Result := FSlidingWindow.MaxSumSubarray(1);
  CheckEquals(10, Result, 'Max sum of subarray with window size 1 should be 10');
  
  // Test with window size equal to array length
  Result := FSlidingWindow.MaxSumSubarray(10);
  CheckEquals(55, Result, 'Max sum of subarray with window size 10 should be 55 (sum of all elements)');
end;

procedure TIntegerSlidingWindowTest.TestMinSumSubarray;
var
  Result: Integer;
begin
  // Test with window size 3
  Result := FSlidingWindow.MinSumSubarray(3);
  CheckEquals(6, Result, 'Min sum of subarray with window size 3 should be 6 (1+2+3)');
  
  // Test with window size 1
  Result := FSlidingWindow.MinSumSubarray(1);
  CheckEquals(1, Result, 'Min sum of subarray with window size 1 should be 1');
end;

procedure TIntegerSlidingWindowTest.TestMaxProductSubarray;
var
  Result: Int64;
begin
  // Test with window size 3
  Result := FSlidingWindow.MaxProductSubarray(3);
  CheckEquals(504, Result, 'Max product of subarray with window size 3 should be 504 (7*8*9)');
end;

procedure TIntegerSlidingWindowTest.TestAverageOfSubarray;
var
  Result: TArray<Double>;
begin
  // Test with window size 3
  Result := FSlidingWindow.AverageOfSubarray(3);
  CheckEquals(8, Length(Result), 'Should have 8 averages for window size 3');
  CheckEquals(2.0, Result[0], 0.001, 'First average should be 2.0 (1+2+3)/3');
  CheckEquals(8.0, Result[7], 0.001, 'Last average should be 8.0 (8+9+10)/3');
end;

procedure TIntegerSlidingWindowTest.TestSlidingWindowMaximum;
var
  Result: TArray<Integer>;
begin
  // Test with window size 3
  Result := FSlidingWindow.SlidingWindowMaximum(3);
  CheckEquals(8, Length(Result), 'Should have 8 maximums for window size 3');
  CheckEquals(3, Result[0], 'First maximum should be 3');
  CheckEquals(10, Result[7], 'Last maximum should be 10');
end;

procedure TIntegerSlidingWindowTest.TestSlidingWindowMinimum;
var
  Result: TArray<Integer>;
begin
  // Test with window size 3
  Result := FSlidingWindow.SlidingWindowMinimum(3);
  CheckEquals(8, Length(Result), 'Should have 8 minimums for window size 3');
  CheckEquals(1, Result[0], 'First minimum should be 1');
  CheckEquals(8, Result[7], 'Last minimum should be 8');
end;

procedure TIntegerSlidingWindowTest.TestCountDistinctInWindow;
var
  Result: TArray<Integer>;
  TestDataWithDuplicates: TArray<Integer>;
  TempWindow: TIntegerSlidingWindow;
begin
  TestDataWithDuplicates := [1, 2, 2, 3, 3, 3, 4, 4, 4, 4];
  TempWindow := TIntegerSlidingWindow.Create(TestDataWithDuplicates);
  try
    Result := TempWindow.CountDistinctInWindow(3);
    CheckEquals(8, Length(Result), 'Should have 8 counts for window size 3');
    CheckEquals(2, Result[0], 'First window should have 2 distinct elements');
  finally
    TempWindow.Free;
  end;
end;

procedure TIntegerSlidingWindowTest.TestLongestSubarrayWithAtMostKDistinct;
var
  Result: Integer;
  TestDataWithDuplicates: TArray<Integer>;
  TempWindow: TIntegerSlidingWindow;
begin
  TestDataWithDuplicates := [1, 2, 1, 2, 3, 1, 2, 3, 4];
  TempWindow := TIntegerSlidingWindow.Create(TestDataWithDuplicates);
  try
    Result := TempWindow.LongestSubarrayWithAtMostKDistinct(2);
    CheckTrue(Result >= 4, 'Should find a subarray with at most 2 distinct elements');
  finally
    TempWindow.Free;
  end;
end;

procedure TIntegerSlidingWindowTest.TestSmallestSubarrayWithSumGreaterThan;
var
  Result: Integer;
begin
  Result := FSlidingWindow.SmallestSubarrayWithSumGreaterThan(15);
  CheckTrue(Result > 0, 'Should find a subarray with sum greater than 15');
  CheckTrue(Result <= 10, 'Result should not exceed array length');
end;

procedure TIntegerSlidingWindowTest.TestLongestSubarrayWithSumEqualsK;
var
  Result: Integer;
begin
  Result := FSlidingWindow.LongestSubarrayWithSumEqualsK(15);
  CheckTrue(Result >= 0, 'Should return non-negative length');
end;

procedure TIntegerSlidingWindowTest.TestMaxLengthSubarrayWithSumK;
var
  Result: Integer;
begin
  Result := FSlidingWindow.MaxLengthSubarrayWithSumK(15);
  CheckTrue(Result >= 0, 'Should return non-negative length');
end;

procedure TIntegerSlidingWindowTest.TestEmptyArray;
var
  EmptyWindow: TIntegerSlidingWindow;
  EmptyData: TArray<Integer>;
  Result: Integer;
begin
  SetLength(EmptyData, 0);
  EmptyWindow := TIntegerSlidingWindow.Create(EmptyData);
  try
    Result := EmptyWindow.MaxSumSubarray(1);
    CheckEquals(0, Result, 'Max sum of empty array should be 0');
  finally
    EmptyWindow.Free;
  end;
end;

procedure TIntegerSlidingWindowTest.TestSingleElement;
var
  SingleWindow: TIntegerSlidingWindow;
  SingleData: TArray<Integer>;
  Result: Integer;
begin
  SingleData := [42];
  SingleWindow := TIntegerSlidingWindow.Create(SingleData);
  try
    Result := SingleWindow.MaxSumSubarray(1);
    CheckEquals(42, Result, 'Max sum of single element should be the element itself');
  finally
    SingleWindow.Free;
  end;
end;

procedure TIntegerSlidingWindowTest.TestWindowSizeGreaterThanArray;
var
  Result: Integer;
begin
  Result := FSlidingWindow.MaxSumSubarray(15); // Window size > array length
  CheckEquals(55, Result, 'Should return sum of all elements when window size > array length');
end;

procedure TIntegerSlidingWindowTest.TestNegativeNumbers;
var
  NegativeWindow: TIntegerSlidingWindow;
  NegativeData: TArray<Integer>;
  Result: Integer;
begin
  NegativeData := [-1, -2, -3, -4, -5];
  NegativeWindow := TIntegerSlidingWindow.Create(NegativeData);
  try
    Result := NegativeWindow.MaxSumSubarray(2);
    CheckEquals(-3, Result, 'Max sum with negative numbers should be -3 (-1 + -2)');
  finally
    NegativeWindow.Free;
  end;
end;

procedure TIntegerSlidingWindowTest.TestAllSameElements;
var
  SameWindow: TIntegerSlidingWindow;
  SameData: TArray<Integer>;
  Result: Integer;
begin
  SameData := [5, 5, 5, 5, 5];
  SameWindow := TIntegerSlidingWindow.Create(SameData);
  try
    Result := SameWindow.MaxSumSubarray(3);
    CheckEquals(15, Result, 'Max sum with same elements should be 15 (5*3)');
  finally
    SameWindow.Free;
  end;
end;

procedure TIntegerSlidingWindowTest.TestPerformanceWithLargeData;
var
  LargeData: TArray<Integer>;
  LargeWindow: TIntegerSlidingWindow;
  I: Integer;
  StartTime, EndTime: TDateTime;
  ExecutionTime: Double;
begin
  // Generate large dataset
  SetLength(LargeData, 100000);
  for I := 0 to 99999 do
    LargeData[I] := Random(1000) + 1;
  
  LargeWindow := TIntegerSlidingWindow.Create(LargeData);
  try
    StartTime := Now;
    LargeWindow.MaxSumSubarray(1000);
    EndTime := Now;
    
    ExecutionTime := MilliSecondsBetween(EndTime, StartTime);
    CheckTrue(ExecutionTime < 1000, 'Large data processing should complete within 1 second');
  finally
    LargeWindow.Free;
  end;
end;

procedure TIntegerSlidingWindowTest.TestMemoryUsage;
var
  MemoryTracker: TMemoryTracker;
  InitialMemory, FinalMemory: Double;
begin
  MemoryTracker := TMemoryTracker.Create;
  try
    InitialMemory := MemoryTracker.GetMemoryUsageMB;
    
    // Perform memory-intensive operations
    FSlidingWindow.SlidingWindowMaximum(5);
    FSlidingWindow.AverageOfSubarray(3);
    
    FinalMemory := MemoryTracker.GetPeakMemoryUsageMB;
    
    // Memory usage should be reasonable (less than 10MB for this test)
    CheckTrue(FinalMemory - InitialMemory < 10, 'Memory usage should be reasonable');
  finally
    MemoryTracker.Free;
  end;
end;

{ TStringSlidingWindowTest }

procedure TStringSlidingWindowTest.SetUp;
begin
  inherited SetUp;
  FTestString := 'abcabcbb';
  FStringSlidingWindow := TStringSlidingWindow.Create(FTestString);
  FPerformanceAnalyzer := TPerformanceAnalyzer.Create;
end;

procedure TStringSlidingWindowTest.TearDown;
begin
  FStringSlidingWindow.Free;
  FPerformanceAnalyzer.Free;
  inherited TearDown;
end;

procedure TStringSlidingWindowTest.TestLongestSubstringWithoutRepeatingChars;
var
  Result: Integer;
begin
  Result := FStringSlidingWindow.LongestSubstringWithoutRepeatingChars;
  CheckEquals(3, Result, 'Longest substring without repeating chars should be 3 ("abc")');
end;

procedure TStringSlidingWindowTest.TestLongestSubstringWithAtMostKDistinctChars;
var
  Result: Integer;
begin
  Result := FStringSlidingWindow.LongestSubstringWithAtMostKDistinctChars(2);
  CheckTrue(Result >= 2, 'Should find substring with at most 2 distinct characters');
end;

procedure TStringSlidingWindowTest.TestMinimumWindowSubstring;
var
  Result: string;
  TestWindow: TStringSlidingWindow;
begin
  TestWindow := TStringSlidingWindow.Create('ADOBECODEBANC');
  try
    Result := TestWindow.MinimumWindowSubstring('ABC');
    CheckEquals('BANC', Result, 'Minimum window substring should be "BANC"');
  finally
    TestWindow.Free;
  end;
end;

procedure TStringSlidingWindowTest.TestLongestRepeatingCharacterReplacement;
var
  Result: Integer;
  TestWindow: TStringSlidingWindow;
begin
  TestWindow := TStringSlidingWindow.Create('AABABBA');
  try
    Result := TestWindow.LongestRepeatingCharacterReplacement('A', 1);
    CheckTrue(Result >= 4, 'Should find longest repeating character replacement');
  finally
    TestWindow.Free;
  end;
end;

procedure TStringSlidingWindowTest.TestPermutationInString;
var
  Result: Boolean;
  TestWindow: TStringSlidingWindow;
begin
  TestWindow := TStringSlidingWindow.Create('eidbaooo');
  try
    Result := TestWindow.PermutationInString('ab');
    CheckTrue(Result, 'Should find permutation "ab" in string');
  finally
    TestWindow.Free;
  end;
end;

procedure TStringSlidingWindowTest.TestFindAllAnagrams;
var
  Result: TArray<Integer>;
  TestWindow: TStringSlidingWindow;
begin
  TestWindow := TStringSlidingWindow.Create('abab');
  try
    Result := TestWindow.FindAllAnagrams('ab');
    CheckEquals(3, Length(Result), 'Should find 3 anagram positions');
  finally
    TestWindow.Free;
  end;
end;

procedure TStringSlidingWindowTest.TestEmptyString;
var
  EmptyWindow: TStringSlidingWindow;
  Result: Integer;
begin
  EmptyWindow := TStringSlidingWindow.Create('');
  try
    Result := EmptyWindow.LongestSubstringWithoutRepeatingChars;
    CheckEquals(0, Result, 'Empty string should return 0');
  finally
    EmptyWindow.Free;
  end;
end;

procedure TStringSlidingWindowTest.TestSingleCharacter;
var
  SingleWindow: TStringSlidingWindow;
  Result: Integer;
begin
  SingleWindow := TStringSlidingWindow.Create('a');
  try
    Result := SingleWindow.LongestSubstringWithoutRepeatingChars;
    CheckEquals(1, Result, 'Single character should return 1');
  finally
    SingleWindow.Free;
  end;
end;

procedure TStringSlidingWindowTest.TestAllSameCharacters;
var
  SameWindow: TStringSlidingWindow;
  Result: Integer;
begin
  SameWindow := TStringSlidingWindow.Create('aaaa');
  try
    Result := SameWindow.LongestSubstringWithoutRepeatingChars;
    CheckEquals(1, Result, 'All same characters should return 1');
  finally
    SameWindow.Free;
  end;
end;

procedure TStringSlidingWindowTest.TestNoValidWindow;
var
  Result: string;
begin
  Result := FStringSlidingWindow.MinimumWindowSubstring('xyz');
  CheckEquals('', Result, 'Should return empty string when no valid window exists');
end;

procedure TStringSlidingWindowTest.TestStringPerformanceWithLargeData;
var
  LargeString: string;
  LargeWindow: TStringSlidingWindow;
  I: Integer;
  StartTime, EndTime: TDateTime;
  ExecutionTime: Double;
begin
  // Generate large string
  LargeString := '';
  for I := 1 to 10000 do
    LargeString := LargeString + Chr(Ord('a') + (I mod 26));
  
  LargeWindow := TStringSlidingWindow.Create(LargeString);
  try
    StartTime := Now;
    LargeWindow.LongestSubstringWithoutRepeatingChars;
    EndTime := Now;
    
    ExecutionTime := MilliSecondsBetween(EndTime, StartTime);
    CheckTrue(ExecutionTime < 2000, 'Large string processing should complete within 2 seconds');
  finally
    LargeWindow.Free;
  end;
end;

{ TGenericSlidingWindowTest }

procedure TGenericSlidingWindowTest.SetUp;
begin
  inherited SetUp;
  FDoubleData := [1.5, 2.5, 3.5, 4.5, 5.5];
  FGenericWindow := TSlidingWindow<Double>.Create(FDoubleData);
end;

procedure TGenericSlidingWindowTest.TearDown;
begin
  FGenericWindow.Free;
  inherited TearDown;
end;

procedure TGenericSlidingWindowTest.TestGenericMaxSum;
var
  Result: Double;
begin
  Result := FGenericWindow.MaxSum(3);
  CheckEquals(13.5, Result, 0.001, 'Max sum should be 13.5 (3.5+4.5+5.5)');
end;

procedure TGenericSlidingWindowTest.TestGenericMinSum;
var
  Result: Double;
begin
  Result := FGenericWindow.MinSum(3);
  CheckEquals(7.5, Result, 0.001, 'Min sum should be 7.5 (1.5+2.5+3.5)');
end;

procedure TGenericSlidingWindowTest.TestGenericAverage;
var
  Result: TArray<Double>;
begin
  Result := FGenericWindow.Average(2);
  CheckEquals(4, Length(Result), 'Should have 4 averages');
  CheckEquals(2.0, Result[0], 0.001, 'First average should be 2.0');
end;

procedure TGenericSlidingWindowTest.TestGenericCustomFunction;
var
  Result: TArray<Double>;
  MaxFunc: TWindowFunction<Double>;
begin
  MaxFunc := function(const Window: TArray<Double>): Double
  var
    I: Integer;
    Max: Double;
  begin
    if Length(Window) = 0 then Exit(0);
    Max := Window[0];
    for I := 1 to High(Window) do
      if Window[I] > Max then Max := Window[I];
    Result := Max;
  end;
  
  Result := FGenericWindow.ApplyFunction(MaxFunc, 2);
  CheckEquals(4, Length(Result), 'Should have 4 results');
  CheckEquals(2.5, Result[0], 0.001, 'First max should be 2.5');
end;

{ TIntegrationTestSuite }

procedure TIntegrationTestSuite.SetUp;
begin
  inherited SetUp;
  FBenchmark := TSlidingWindowBenchmark.Create;
end;

procedure TIntegrationTestSuite.TearDown;
begin
  FBenchmark.Free;
  inherited TearDown;
end;

procedure TIntegrationTestSuite.TestFullBenchmarkSuite;
begin
  FBenchmark.RunFullBenchmarkSuite;
  CheckTrue(FBenchmark.Analyzer.GetMeasurementCount > 0, 'Should have measurements after running benchmark suite');
end;

procedure TIntegrationTestSuite.TestScalabilityTest;
begin
  FBenchmark.RunScalabilityTest(100, 1000, 300);
  CheckTrue(FBenchmark.Analyzer.GetMeasurementCount > 0, 'Should have measurements after scalability test');
end;

procedure TIntegrationTestSuite.TestReportGeneration;
var
  Report: string;
begin
  FBenchmark.RunFullBenchmarkSuite;
  Report := FBenchmark.GetBenchmarkResults;
  CheckTrue(Length(Report) > 0, 'Report should not be empty');
  CheckTrue(Pos('Performance Report', Report) > 0, 'Report should contain performance information');
end;

procedure TIntegrationTestSuite.TestCSVReportGeneration;
var
  CSV: string;
begin
  FBenchmark.RunFullBenchmarkSuite;
  CSV := FBenchmark.Analyzer.GenerateCSVReport;
  CheckTrue(Length(CSV) > 0, 'CSV report should not be empty');
  CheckTrue(Pos('Algorithm,InputSize', CSV) > 0, 'CSV should contain proper headers');
end;

procedure TIntegerSlidingWindowTest.TestKthSmallestAtPosition;
var
  Result: Integer;
begin
  // Test data: [1, 3, 2, 5, 4, 6, 8, 7, 9, 10]
  // Window at position 0, size 3: [1, 3, 2] -> sorted: [1, 2, 3]
  Result := FSlidingWindow.KthSmallestAtPosition(0, 3, 1); // 1st smallest
  CheckEquals(1, Result, '1st smallest in first window should be 1');
  
  Result := FSlidingWindow.KthSmallestAtPosition(0, 3, 2); // 2nd smallest
  CheckEquals(2, Result, '2nd smallest in first window should be 2');
  
  Result := FSlidingWindow.KthSmallestAtPosition(0, 3, 3); // 3rd smallest
  CheckEquals(3, Result, '3rd smallest in first window should be 3');
  
  // Window at position 2, size 3: [2, 5, 4] -> sorted: [2, 4, 5]
  Result := FSlidingWindow.KthSmallestAtPosition(2, 3, 2); // 2nd smallest
  CheckEquals(4, Result, '2nd smallest in window [2,5,4] should be 4');
  
  // Test edge cases
  Result := FSlidingWindow.KthSmallestAtPosition(-1, 3, 1); // Invalid position
  CheckEquals(0, Result, 'Invalid position should return 0');
  
  Result := FSlidingWindow.KthSmallestAtPosition(0, 3, 0); // Invalid k
  CheckEquals(0, Result, 'Invalid k should return 0');
  
  Result := FSlidingWindow.KthSmallestAtPosition(0, 3, 4); // k > window size
  CheckEquals(0, Result, 'k > window size should return 0');
end;

procedure TIntegerSlidingWindowTest.TestKthSmallestInSlidingWindow;
var
  Result: TArray<Integer>;
begin
  // Test data: [1, 3, 2, 5, 4, 6, 8, 7, 9, 10]
  // Window size 3, k=2 (2nd smallest in each window)
  Result := FSlidingWindow.KthSmallestInSlidingWindow(3, 2);
  
  CheckEquals(8, Length(Result), 'Should have 8 windows for size 3');
  
  // Window 0: [1, 3, 2] -> sorted: [1, 2, 3] -> 2nd smallest = 2
  CheckEquals(2, Result[0], '2nd smallest in window [1,3,2] should be 2');
  
  // Window 1: [3, 2, 5] -> sorted: [2, 3, 5] -> 2nd smallest = 3
  CheckEquals(3, Result[1], '2nd smallest in window [3,2,5] should be 3');
  
  // Window 2: [2, 5, 4] -> sorted: [2, 4, 5] -> 2nd smallest = 4
  CheckEquals(4, Result[2], '2nd smallest in window [2,5,4] should be 4');
  
  // Test with k=1 (minimum in each window)
  Result := FSlidingWindow.KthSmallestInSlidingWindow(3, 1);
  CheckEquals(1, Result[0], '1st smallest in window [1,3,2] should be 1');
  CheckEquals(2, Result[1], '1st smallest in window [3,2,5] should be 2');
  CheckEquals(2, Result[2], '1st smallest in window [2,5,4] should be 2');
  
  // Test with k=3 (maximum in each window)
  Result := FSlidingWindow.KthSmallestInSlidingWindow(3, 3);
  CheckEquals(3, Result[0], '3rd smallest in window [1,3,2] should be 3');
  CheckEquals(5, Result[1], '3rd smallest in window [3,2,5] should be 5');
  CheckEquals(5, Result[2], '3rd smallest in window [2,5,4] should be 5');
  
  // Test edge cases
  Result := FSlidingWindow.KthSmallestInSlidingWindow(0, 1); // Invalid window size
  CheckEquals(0, Length(Result), 'Invalid window size should return empty array');
  
  Result := FSlidingWindow.KthSmallestInSlidingWindow(3, 0); // Invalid k
  CheckEquals(0, Length(Result), 'Invalid k should return empty array');
  
  Result := FSlidingWindow.KthSmallestInSlidingWindow(15, 1); // Window size > data size
  CheckEquals(0, Length(Result), 'Window size > data size should return empty array');
end;

// Test suite registration
initialization
  RegisterTest('Sliding Window Tests', TIntegerSlidingWindowTest.Suite);
  RegisterTest('String Sliding Window Tests', TStringSlidingWindowTest.Suite);
  RegisterTest('Generic Sliding Window Tests', TGenericSlidingWindowTest.Suite);
  RegisterTest('Integration Tests', TIntegrationTestSuite.Suite);

end.