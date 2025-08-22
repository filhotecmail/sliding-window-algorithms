program SlidingWindowDemo;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, DateUtils,
  SlidingWindow, PerformanceAnalyzer;

var
  IntegerWindow: TIntegerSlidingWindow;
  StringWindow: TStringSlidingWindow;
  Benchmark: TSlidingWindowBenchmark;
  TestData: TArray<Integer>;
  TestString: string;
  i: Integer;
  Result: Integer;
  SubStr: string;
  Report: string;
  KthResults: TArray<Integer>;
  Results: TArray<Integer>;

begin
  WriteLn('========================================');
  WriteLn('Sliding Window Algorithms - Demo');
  WriteLn('========================================');
  WriteLn('');
  
  try
    // Demo 1: Fixed-size sliding window with integers
    WriteLn('Demo 1: Fixed-size Sliding Window (Integers)');
    WriteLn('-------------------------------------------');
    
    // Create test data
    SetLength(TestData, 10);
    for i := 0 to High(TestData) do
      TestData[i] := Random(100) + 1;
    
    Write('Test data: ');
    for i := 0 to High(TestData) do
    begin
      Write(TestData[i]);
      if i < High(TestData) then Write(', ');
    end;
    WriteLn('');
    
    IntegerWindow := TSlidingWindowInteger.Create;
    try
      // Find maximum sum in window of size 3
      Result := IntegerWindow.MaxSumFixedWindow(TestData, 3);
      WriteLn('Maximum sum in window of size 3: ', Result);
      
      // Find minimum sum in window of size 3
      Result := IntegerWindow.MinSumFixedWindow(TestData, 3);
      WriteLn('Minimum sum in window of size 3: ', Result);
      
      // Find maximum element in each window
      WriteLn('Maximum in each window of size 3:');
      for i := 0 to Length(TestData) - 3 do
      begin
        Result := IntegerWindow.MaxInWindow(Copy(TestData, i, 3));
        WriteLn('  Window [', i, '-', i+2, ']: ', Result);
      end;
    finally
      IntegerWindow.Free;
    end;
    
    WriteLn('');
    
    // Demo 2: Variable-size sliding window
    WriteLn('Demo 2: Variable-size Sliding Window');
    WriteLn('-----------------------------------');
    
    IntegerWindow := TSlidingWindowInteger.Create;
    try
      // Find smallest window with sum >= target
  Result := IntegerWindow.SmallestSubarrayWithSumGreaterThan(150);
  if Result > 0 then
    WriteLn('Smallest window with sum >= 150: ', Result, ' elements')
  else
    WriteLn('No window found with sum >= 150');
    
  // P. Raykov Algorithm 1: k-th smallest element in sliding window
  WriteLn;
  WriteLn('=== P. Raykov k-th Smallest Element Algorithm ===');
  
  // Find 2nd smallest element in each window of size 5
  KthResults := IntegerWindow.KthSmallestInSlidingWindow(5, 2);
  Write('2nd smallest in each window (size 5): ');
  for I := 0 to High(KthResults) do
  begin
    Write(KthResults[I]);
    if I < High(KthResults) then Write(', ');
  end;
  WriteLn;
  
  // Find median (middle element) in each window of size 5
  KthResults := IntegerWindow.KthSmallestInSlidingWindow(5, 3); // 3rd element is median for size 5
  Write('Median in each window (size 5): ');
  for I := 0 to High(KthResults) do
  begin
    Write(KthResults[I]);
    if I < High(KthResults) then Write(', ');
  end;
  WriteLn;
  
  // Find specific k-th smallest at a specific position
  Result := IntegerWindow.KthSmallestAtPosition(0, 7, 4); // 4th smallest in first 7 elements
  WriteLn('4th smallest in first 7 elements: ', Result);
      
      // Find largest window with sum <= target
      Result := IntegerWindow.LargestWindowWithSum(TestData, 100);
      if Result > 0 then
        WriteLn('Largest window with sum <= 100: ', Result, ' elements')
      else
        WriteLn('No window found with sum <= 100');
    finally
      IntegerWindow.Free;
    end;
    
    WriteLn('');
    
    // Demo 3: String sliding window
    WriteLn('Demo 3: String Sliding Window');
    WriteLn('-----------------------------');
    
    TestString := 'abcabcbb';
    WriteLn('Test string: "', TestString, '"');
    
    StringWindow := TSlidingWindowString.Create;
    try
      // Find longest substring without repeating characters
      SubStr := StringWindow.LongestSubstringWithoutRepeating(TestString);
      WriteLn('Longest substring without repeating chars: "', SubStr, '" (length: ', Length(SubStr), ')');
      
      // Find longest substring with at most K distinct characters
      SubStr := StringWindow.LongestSubstringWithKDistinct(TestString, 2);
      WriteLn('Longest substring with at most 2 distinct chars: "', SubStr, '" (length: ', Length(SubStr), ')');
      
      // Check if string contains permutation of pattern
      if StringWindow.ContainsPermutation(TestString, 'abc') then
        WriteLn('String contains permutation of "abc": Yes')
      else
        WriteLn('String contains permutation of "abc": No');
    finally
      StringWindow.Free;
    end;
    
    WriteLn('');
    
    // Demo 4: Performance benchmark
    WriteLn('Demo 4: Performance Benchmark');
    WriteLn('-----------------------------');
    
    Benchmark := TSlidingWindowBenchmark.Create;
    try
      WriteLn('Running quick benchmark...');
      
      // Run a quick benchmark
      Benchmark.BenchmarkIntegerAlgorithms(1000, 100, 10);
      Benchmark.BenchmarkStringAlgorithms(1000, 10);
      
      // Get and display results
      Report := Benchmark.GetBenchmarkResults;
      WriteLn('');
      WriteLn('Benchmark Results:');
      WriteLn(Report);
      
    finally
      Benchmark.Free;
    end;
    
    WriteLn('');
    WriteLn('========================================');
    WriteLn('Demo completed successfully!');
    WriteLn('========================================');
    
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
  
  WriteLn('');
  WriteLn('Press Enter to exit...');
  ReadLn;
end.