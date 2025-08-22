unit PerformanceAnalyzer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Generics.Collections;

type
  // Performance measurement record
  TPerformanceMeasurement = record
    AlgorithmName: string;
    InputSize: Integer;
    WindowSize: Integer;
    ExecutionTimeMs: Double;
    MemoryUsedMB: Double;
    Result: Variant;
    TimeComplexity: string;
    SpaceComplexity: string;
  end;

  // Performance analyzer class
  TPerformanceAnalyzer = class
  private
    FMeasurements: TList<TPerformanceMeasurement>;
    FStartTime: TDateTime;
    FStartMemory: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Measurement methods
    procedure StartMeasurement;
    function EndMeasurement(const AlgorithmName: string; InputSize, WindowSize: Integer;
      const Result: Variant; const TimeComplexity: string = 'O(n)';
      const SpaceComplexity: string = 'O(1)'): TPerformanceMeasurement;
    
    // Analysis methods
    procedure AddMeasurement(const Measurement: TPerformanceMeasurement);
    function GetAverageExecutionTime(const AlgorithmName: string): Double;
    function GetBestPerformingAlgorithm: string;
    function GetWorstPerformingAlgorithm: string;
    
    // Reporting methods
    function GenerateReport: string;
    function GenerateCSVReport: string;
    procedure SaveReportToFile(const FileName: string);
    procedure SaveCSVReportToFile(const FileName: string);
    
    // Comparison methods
    function CompareAlgorithms(const Algorithm1, Algorithm2: string): string;
    function GetComplexityAnalysis: string;
    
    // Utility methods
    procedure ClearMeasurements;
    function GetMeasurementCount: Integer;
    function GetMeasurement(Index: Integer): TPerformanceMeasurement;
    
    property Measurements: TList<TPerformanceMeasurement> read FMeasurements;
  end;

  // Benchmark suite for sliding window algorithms
  TSlidingWindowBenchmark = class
  private
    FAnalyzer: TPerformanceAnalyzer;
    FTestData: TArray<Integer>;
    FTestStrings: TArray<string>;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Data generation methods
    procedure GenerateRandomIntegerData(Size: Integer; MinValue: Integer = 1; MaxValue: Integer = 1000);
    procedure GenerateRandomStringData(Count: Integer; MinLength: Integer = 10; MaxLength: Integer = 100);
    procedure GenerateWorstCaseData(Size: Integer);
    procedure GenerateBestCaseData(Size: Integer);
    
    // Benchmark methods for integer algorithms
    procedure BenchmarkMaxSumSubarray(WindowSizes: TArray<Integer>);
    procedure BenchmarkMinSumSubarray(WindowSizes: TArray<Integer>);
    procedure BenchmarkSlidingWindowMaximum(WindowSizes: TArray<Integer>);
    procedure BenchmarkVariableSizeAlgorithms;
    
    // Benchmark methods for string algorithms
    procedure BenchmarkStringAlgorithms;
    
    // Benchmark methods for k-th smallest algorithms
    procedure BenchmarkKthSmallestAlgorithms;
    
    // Comprehensive benchmark suite
    procedure RunFullBenchmarkSuite;
    procedure RunScalabilityTest(StartSize, EndSize, Step: Integer);
    
    // Results and reporting
    function GetBenchmarkResults: string;
    procedure SaveBenchmarkResults(const FileName: string);
    
    property Analyzer: TPerformanceAnalyzer read FAnalyzer;
    property TestData: TArray<Integer> read FTestData;
    property TestStrings: TArray<string> read FTestStrings;
  end;

  // Memory usage tracker
  TMemoryTracker = class
  private
    FInitialMemory: Cardinal;
    FPeakMemory: Cardinal;
    FCurrentMemory: Cardinal;
  public
    constructor Create;
    
    procedure StartTracking;
    procedure UpdatePeakMemory;
    function GetMemoryUsage: Cardinal;
    function GetPeakMemoryUsage: Cardinal;
    function GetMemoryUsageMB: Double;
    function GetPeakMemoryUsageMB: Double;
    
    property InitialMemory: Cardinal read FInitialMemory;
    property PeakMemory: Cardinal read FPeakMemory;
    property CurrentMemory: Cardinal read FCurrentMemory;
  end;

implementation

uses
  SlidingWindow, Math;

{ TPerformanceAnalyzer }

constructor TPerformanceAnalyzer.Create;
begin
  inherited Create;
  FMeasurements := TList<TPerformanceMeasurement>.Create;
end;

destructor TPerformanceAnalyzer.Destroy;
begin
  FMeasurements.Free;
  inherited Destroy;
end;

procedure TPerformanceAnalyzer.StartMeasurement;
begin
  FStartTime := Now;
  FStartMemory := GetHeapStatus.TotalAllocated;
end;

function TPerformanceAnalyzer.EndMeasurement(const AlgorithmName: string;
  InputSize, WindowSize: Integer; const Result: Variant;
  const TimeComplexity, SpaceComplexity: string): TPerformanceMeasurement;
var
  EndTime: TDateTime;
  EndMemory: Cardinal;
begin
  EndTime := Now;
  EndMemory := GetHeapStatus.TotalAllocated;
  
  Result.AlgorithmName := AlgorithmName;
  Result.InputSize := InputSize;
  Result.WindowSize := WindowSize;
  Result.ExecutionTimeMs := MilliSecondsBetween(EndTime, FStartTime);
  Result.MemoryUsedMB := (EndMemory - FStartMemory) / (1024 * 1024);
  Result.Result := Result;
  Result.TimeComplexity := TimeComplexity;
  Result.SpaceComplexity := SpaceComplexity;
  
  FMeasurements.Add(Result);
end;

procedure TPerformanceAnalyzer.AddMeasurement(const Measurement: TPerformanceMeasurement);
begin
  FMeasurements.Add(Measurement);
end;

function TPerformanceAnalyzer.GetAverageExecutionTime(const AlgorithmName: string): Double;
var
  Measurement: TPerformanceMeasurement;
  TotalTime: Double;
  Count: Integer;
begin
  TotalTime := 0;
  Count := 0;
  
  for Measurement in FMeasurements do
  begin
    if Measurement.AlgorithmName = AlgorithmName then
    begin
      TotalTime := TotalTime + Measurement.ExecutionTimeMs;
      Inc(Count);
    end;
  end;
  
  if Count > 0 then
    Result := TotalTime / Count
  else
    Result := 0;
end;

function TPerformanceAnalyzer.GetBestPerformingAlgorithm: string;
var
  Measurement: TPerformanceMeasurement;
  BestTime: Double;
  BestAlgorithm: string;
begin
  BestTime := MaxDouble;
  BestAlgorithm := '';
  
  for Measurement in FMeasurements do
  begin
    if Measurement.ExecutionTimeMs < BestTime then
    begin
      BestTime := Measurement.ExecutionTimeMs;
      BestAlgorithm := Measurement.AlgorithmName;
    end;
  end;
  
  Result := BestAlgorithm;
end;

function TPerformanceAnalyzer.GetWorstPerformingAlgorithm: string;
var
  Measurement: TPerformanceMeasurement;
  WorstTime: Double;
  WorstAlgorithm: string;
begin
  WorstTime := 0;
  WorstAlgorithm := '';
  
  for Measurement in FMeasurements do
  begin
    if Measurement.ExecutionTimeMs > WorstTime then
    begin
      WorstTime := Measurement.ExecutionTimeMs;
      WorstAlgorithm := Measurement.AlgorithmName;
    end;
  end;
  
  Result := WorstAlgorithm;
end;

function TPerformanceAnalyzer.GenerateReport: string;
var
  Report: TStringBuilder;
  Measurement: TPerformanceMeasurement;
  I: Integer;
begin
  Report := TStringBuilder.Create;
  try
    Report.AppendLine('=== Sliding Window Algorithms Performance Report ===');
    Report.AppendLine('');
    Report.AppendLine(Format('Total Measurements: %d', [FMeasurements.Count]));
    Report.AppendLine(Format('Best Performing Algorithm: %s', [GetBestPerformingAlgorithm]));
    Report.AppendLine(Format('Worst Performing Algorithm: %s', [GetWorstPerformingAlgorithm]));
    Report.AppendLine('');
    Report.AppendLine('Detailed Measurements:');
    Report.AppendLine('----------------------');
    
    for I := 0 to FMeasurements.Count - 1 do
    begin
      Measurement := FMeasurements[I];
      Report.AppendLine(Format('%d. %s', [I + 1, Measurement.AlgorithmName]));
      Report.AppendLine(Format('   Input Size: %d', [Measurement.InputSize]));
      Report.AppendLine(Format('   Window Size: %d', [Measurement.WindowSize]));
      Report.AppendLine(Format('   Execution Time: %.2f ms', [Measurement.ExecutionTimeMs]));
      Report.AppendLine(Format('   Memory Used: %.2f MB', [Measurement.MemoryUsedMB]));
      Report.AppendLine(Format('   Time Complexity: %s', [Measurement.TimeComplexity]));
      Report.AppendLine(Format('   Space Complexity: %s', [Measurement.SpaceComplexity]));
      Report.AppendLine('');
    end;
    
    Result := Report.ToString;
  finally
    Report.Free;
  end;
end;

function TPerformanceAnalyzer.GenerateCSVReport: string;
var
  CSV: TStringBuilder;
  Measurement: TPerformanceMeasurement;
begin
  CSV := TStringBuilder.Create;
  try
    // Header
    CSV.AppendLine('Algorithm,InputSize,WindowSize,ExecutionTime(ms),MemoryUsed(MB),TimeComplexity,SpaceComplexity');
    
    // Data rows
    for Measurement in FMeasurements do
    begin
      CSV.AppendLine(Format('%s,%d,%d,%.2f,%.2f,%s,%s', [
        Measurement.AlgorithmName,
        Measurement.InputSize,
        Measurement.WindowSize,
        Measurement.ExecutionTimeMs,
        Measurement.MemoryUsedMB,
        Measurement.TimeComplexity,
        Measurement.SpaceComplexity
      ]));
    end;
    
    Result := CSV.ToString;
  finally
    CSV.Free;
  end;
end;

procedure TPerformanceAnalyzer.SaveReportToFile(const FileName: string);
var
  Report: string;
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  Report := GenerateReport;
  StringStream := TStringStream.Create(Report, TEncoding.UTF8);
  try
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      FileStream.CopyFrom(StringStream, 0);
    finally
      FileStream.Free;
    end;
  finally
    StringStream.Free;
  end;
end;

procedure TPerformanceAnalyzer.SaveCSVReportToFile(const FileName: string);
var
  CSV: string;
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  CSV := GenerateCSVReport;
  StringStream := TStringStream.Create(CSV, TEncoding.UTF8);
  try
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      FileStream.CopyFrom(StringStream, 0);
    finally
      FileStream.Free;
    end;
  finally
    StringStream.Free;
  end;
end;

function TPerformanceAnalyzer.CompareAlgorithms(const Algorithm1, Algorithm2: string): string;
var
  Avg1, Avg2: Double;
  Report: TStringBuilder;
begin
  Avg1 := GetAverageExecutionTime(Algorithm1);
  Avg2 := GetAverageExecutionTime(Algorithm2);
  
  Report := TStringBuilder.Create;
  try
    Report.AppendLine(Format('Algorithm Comparison: %s vs %s', [Algorithm1, Algorithm2]));
    Report.AppendLine(Format('%s Average Time: %.2f ms', [Algorithm1, Avg1]));
    Report.AppendLine(Format('%s Average Time: %.2f ms', [Algorithm2, Avg2]));
    
    if Avg1 < Avg2 then
      Report.AppendLine(Format('%s is %.2f%% faster than %s', [
        Algorithm1, ((Avg2 - Avg1) / Avg2) * 100, Algorithm2]))
    else if Avg2 < Avg1 then
      Report.AppendLine(Format('%s is %.2f%% faster than %s', [
        Algorithm2, ((Avg1 - Avg2) / Avg1) * 100, Algorithm1]))
    else
      Report.AppendLine('Both algorithms have similar performance');
    
    Result := Report.ToString;
  finally
    Report.Free;
  end;
end;

function TPerformanceAnalyzer.GetComplexityAnalysis: string;
var
  Report: TStringBuilder;
  Measurement: TPerformanceMeasurement;
  ComplexityGroups: TDictionary<string, TList<TPerformanceMeasurement>>;
  Key: string;
  Group: TList<TPerformanceMeasurement>;
begin
  ComplexityGroups := TDictionary<string, TList<TPerformanceMeasurement>>.Create;
  try
    // Group measurements by time complexity
    for Measurement in FMeasurements do
    begin
      if not ComplexityGroups.ContainsKey(Measurement.TimeComplexity) then
        ComplexityGroups.Add(Measurement.TimeComplexity, TList<TPerformanceMeasurement>.Create);
      ComplexityGroups[Measurement.TimeComplexity].Add(Measurement);
    end;
    
    Report := TStringBuilder.Create;
    try
      Report.AppendLine('=== Complexity Analysis ===');
      Report.AppendLine('');
      
      for Key in ComplexityGroups.Keys do
      begin
        Group := ComplexityGroups[Key];
        Report.AppendLine(Format('Time Complexity: %s (%d algorithms)', [Key, Group.Count]));
        
        for Measurement in Group do
        begin
          Report.AppendLine(Format('  - %s: %.2f ms (Input: %d, Window: %d)', [
            Measurement.AlgorithmName,
            Measurement.ExecutionTimeMs,
            Measurement.InputSize,
            Measurement.WindowSize
          ]));
        end;
        Report.AppendLine('');
      end;
      
      Result := Report.ToString;
    finally
      Report.Free;
    end;
  finally
    for Group in ComplexityGroups.Values do
      Group.Free;
    ComplexityGroups.Free;
  end;
end;

procedure TPerformanceAnalyzer.ClearMeasurements;
begin
  FMeasurements.Clear;
end;

function TPerformanceAnalyzer.GetMeasurementCount: Integer;
begin
  Result := FMeasurements.Count;
end;

function TPerformanceAnalyzer.GetMeasurement(Index: Integer): TPerformanceMeasurement;
begin
  if (Index >= 0) and (Index < FMeasurements.Count) then
    Result := FMeasurements[Index]
  else
    raise Exception.CreateFmt('Index %d out of bounds (0..%d)', [Index, FMeasurements.Count - 1]);
end;

{ TSlidingWindowBenchmark }

constructor TSlidingWindowBenchmark.Create;
begin
  inherited Create;
  FAnalyzer := TPerformanceAnalyzer.Create;
end;

destructor TSlidingWindowBenchmark.Destroy;
begin
  FAnalyzer.Free;
  inherited Destroy;
end;

procedure TSlidingWindowBenchmark.GenerateRandomIntegerData(Size: Integer; MinValue, MaxValue: Integer);
var
  I: Integer;
begin
  SetLength(FTestData, Size);
  Randomize;
  
  for I := 0 to Size - 1 do
    FTestData[I] := Random(MaxValue - MinValue + 1) + MinValue;
end;

procedure TSlidingWindowBenchmark.GenerateRandomStringData(Count: Integer; MinLength, MaxLength: Integer);
var
  I, J, Len: Integer;
  S: string;
begin
  SetLength(FTestStrings, Count);
  Randomize;
  
  for I := 0 to Count - 1 do
  begin
    Len := Random(MaxLength - MinLength + 1) + MinLength;
    S := '';
    for J := 1 to Len do
      S := S + Chr(Ord('a') + Random(26));
    FTestStrings[I] := S;
  end;
end;

procedure TSlidingWindowBenchmark.GenerateWorstCaseData(Size: Integer);
var
  I: Integer;
begin
  SetLength(FTestData, Size);
  
  // Worst case: all elements are the same (for some algorithms)
  for I := 0 to Size - 1 do
    FTestData[I] := 1;
end;

procedure TSlidingWindowBenchmark.GenerateBestCaseData(Size: Integer);
var
  I: Integer;
begin
  SetLength(FTestData, Size);
  
  // Best case: sorted array
  for I := 0 to Size - 1 do
    FTestData[I] := I + 1;
end;

procedure TSlidingWindowBenchmark.BenchmarkMaxSumSubarray(WindowSizes: TArray<Integer>);
var
  SlidingWindow: TIntegerSlidingWindow;
  WindowSize: Integer;
  Result: Integer;
  Measurement: TPerformanceMeasurement;
begin
  SlidingWindow := TIntegerSlidingWindow.Create(FTestData);
  try
    for WindowSize in WindowSizes do
    begin
      FAnalyzer.StartMeasurement;
      Result := SlidingWindow.MaxSumSubarray(WindowSize);
      Measurement := FAnalyzer.EndMeasurement('MaxSumSubarray', Length(FTestData), WindowSize, Result, 'O(n)', 'O(1)');
    end;
  finally
    SlidingWindow.Free;
  end;
end;

procedure TSlidingWindowBenchmark.BenchmarkMinSumSubarray(WindowSizes: TArray<Integer>);
var
  SlidingWindow: TIntegerSlidingWindow;
  WindowSize: Integer;
  Result: Integer;
  Measurement: TPerformanceMeasurement;
begin
  SlidingWindow := TIntegerSlidingWindow.Create(FTestData);
  try
    for WindowSize in WindowSizes do
    begin
      FAnalyzer.StartMeasurement;
      Result := SlidingWindow.MinSumSubarray(WindowSize);
      Measurement := FAnalyzer.EndMeasurement('MinSumSubarray', Length(FTestData), WindowSize, Result, 'O(n)', 'O(1)');
    end;
  finally
    SlidingWindow.Free;
  end;
end;

procedure TSlidingWindowBenchmark.BenchmarkSlidingWindowMaximum(WindowSizes: TArray<Integer>);
var
  SlidingWindow: TIntegerSlidingWindow;
  WindowSize: Integer;
  Result: TArray<Integer>;
  Measurement: TPerformanceMeasurement;
begin
  SlidingWindow := TIntegerSlidingWindow.Create(FTestData);
  try
    for WindowSize in WindowSizes do
    begin
      FAnalyzer.StartMeasurement;
      Result := SlidingWindow.SlidingWindowMaximum(WindowSize);
      Measurement := FAnalyzer.EndMeasurement('SlidingWindowMaximum', Length(FTestData), WindowSize, Length(Result), 'O(n)', 'O(k)');
    end;
  finally
    SlidingWindow.Free;
  end;
end;

procedure TSlidingWindowBenchmark.BenchmarkVariableSizeAlgorithms;
var
  SlidingWindow: TIntegerSlidingWindow;
  Result: Integer;
  Measurement: TPerformanceMeasurement;
begin
  SlidingWindow := TIntegerSlidingWindow.Create(FTestData);
  try
    // Benchmark LongestSubarrayWithAtMostKDistinct
    FAnalyzer.StartMeasurement;
    Result := SlidingWindow.LongestSubarrayWithAtMostKDistinct(3);
    Measurement := FAnalyzer.EndMeasurement('LongestSubarrayWithAtMostKDistinct', Length(FTestData), 0, Result, 'O(n)', 'O(k)');
    
    // Benchmark SmallestSubarrayWithSumGreaterThan
    FAnalyzer.StartMeasurement;
    Result := SlidingWindow.SmallestSubarrayWithSumGreaterThan(100);
    Measurement := FAnalyzer.EndMeasurement('SmallestSubarrayWithSumGreaterThan', Length(FTestData), 0, Result, 'O(n)', 'O(1)');
    
    // Benchmark LongestSubarrayWithSumEqualsK
    FAnalyzer.StartMeasurement;
    Result := SlidingWindow.LongestSubarrayWithSumEqualsK(50);
    Measurement := FAnalyzer.EndMeasurement('LongestSubarrayWithSumEqualsK', Length(FTestData), 0, Result, 'O(n)', 'O(1)');
  finally
    SlidingWindow.Free;
  end;
end;

procedure TSlidingWindowBenchmark.BenchmarkStringAlgorithms;
var
  StringWindow: TStringSlidingWindow;
  TestString: string;
  Result: Integer;
  ResultStr: string;
  Measurement: TPerformanceMeasurement;
begin
  if Length(FTestStrings) = 0 then
    Exit;
  
  TestString := FTestStrings[0];
  StringWindow := TStringSlidingWindow.Create(TestString);
  try
    // Benchmark LongestSubstringWithoutRepeatingChars
    FAnalyzer.StartMeasurement;
    Result := StringWindow.LongestSubstringWithoutRepeatingChars;
    Measurement := FAnalyzer.EndMeasurement('LongestSubstringWithoutRepeatingChars', Length(TestString), 0, Result, 'O(n)', 'O(min(m,n))');
    
    // Benchmark LongestSubstringWithAtMostKDistinctChars
    FAnalyzer.StartMeasurement;
    Result := StringWindow.LongestSubstringWithAtMostKDistinctChars(3);
    Measurement := FAnalyzer.EndMeasurement('LongestSubstringWithAtMostKDistinctChars', Length(TestString), 0, Result, 'O(n)', 'O(k)');
    
    // Benchmark MinimumWindowSubstring
    FAnalyzer.StartMeasurement;
    ResultStr := StringWindow.MinimumWindowSubstring('abc');
    Measurement := FAnalyzer.EndMeasurement('MinimumWindowSubstring', Length(TestString), 0, Length(ResultStr), 'O(|s| + |t|)', 'O(|s| + |t|)');
  finally
    StringWindow.Free;
  end;
end;

procedure TSlidingWindowBenchmark.RunFullBenchmarkSuite;
var
  WindowSizes: TArray<Integer>;
begin
  // Generate test data
  GenerateRandomIntegerData(10000, 1, 1000);
  GenerateRandomStringData(10, 100, 1000);
  
  // Define window sizes to test
  WindowSizes := [5, 10, 50, 100, 500];
  
  // Run integer algorithm benchmarks
  BenchmarkMaxSumSubarray(WindowSizes);
  BenchmarkMinSumSubarray(WindowSizes);
  BenchmarkSlidingWindowMaximum(WindowSizes);
  BenchmarkVariableSizeAlgorithms;
  
  // Run string algorithm benchmarks
  BenchmarkStringAlgorithms;
  
  // Run k-th smallest element benchmarks
  BenchmarkKthSmallestAlgorithms;
end;

procedure TSlidingWindowBenchmark.RunScalabilityTest(StartSize, EndSize, Step: Integer);
var
  Size: Integer;
  WindowSize: Integer;
  SlidingWindow: TIntegerSlidingWindow;
  Result: Integer;
  Measurement: TPerformanceMeasurement;
begin
  Size := StartSize;
  while Size <= EndSize do
  begin
    GenerateRandomIntegerData(Size, 1, 1000);
    WindowSize := Min(Size div 10, 100); // Window size is 10% of data size, max 100
    
    SlidingWindow := TIntegerSlidingWindow.Create(FTestData);
    try
      FAnalyzer.StartMeasurement;
      Result := SlidingWindow.MaxSumSubarray(WindowSize);
      Measurement := FAnalyzer.EndMeasurement(Format('MaxSumSubarray_Size_%d', [Size]), Size, WindowSize, Result, 'O(n)', 'O(1)');
    finally
      SlidingWindow.Free;
    end;
    
    Inc(Size, Step);
  end;
end;

function TSlidingWindowBenchmark.GetBenchmarkResults: string;
begin
  Result := FAnalyzer.GenerateReport;
end;

procedure TSlidingWindowBenchmark.SaveBenchmarkResults(const FileName: string);
begin
  FAnalyzer.SaveReportToFile(FileName);
end;

{ TMemoryTracker }

constructor TMemoryTracker.Create;
begin
  inherited Create;
  StartTracking;
end;

procedure TMemoryTracker.StartTracking;
begin
  FInitialMemory := GetHeapStatus.TotalAllocated;
  FPeakMemory := FInitialMemory;
  FCurrentMemory := FInitialMemory;
end;

procedure TMemoryTracker.UpdatePeakMemory;
begin
  FCurrentMemory := GetHeapStatus.TotalAllocated;
  if FCurrentMemory > FPeakMemory then
    FPeakMemory := FCurrentMemory;
end;

function TMemoryTracker.GetMemoryUsage: Cardinal;
begin
  FCurrentMemory := GetHeapStatus.TotalAllocated;
  Result := FCurrentMemory - FInitialMemory;
end;

function TMemoryTracker.GetPeakMemoryUsage: Cardinal;
begin
  UpdatePeakMemory;
  Result := FPeakMemory - FInitialMemory;
end;

function TMemoryTracker.GetMemoryUsageMB: Double;
begin
  Result := GetMemoryUsage / (1024 * 1024);
end;

function TMemoryTracker.GetPeakMemoryUsageMB: Double;
begin
  Result := GetPeakMemoryUsage / (1024 * 1024);
end;

procedure TSlidingWindowBenchmark.BenchmarkKthSmallestAlgorithms;
var
  SlidingWindow: TIntegerSlidingWindow;
  WindowSizes: TArray<Integer>;
  KValues: TArray<Integer>;
  WindowSize, K: Integer;
  Results: TArray<Integer>;
  SingleResult: Integer;
  Measurement: TPerformanceMeasurement;
begin
  SlidingWindow := TIntegerSlidingWindow.Create(FTestData);
  try
    WindowSizes := [3, 5, 10, 20, 50];
    KValues := [1, 2, 3];
    
    WriteLn('Benchmarking P. Raykov k-th Smallest Element Algorithms...');
    
    for WindowSize in WindowSizes do
    begin
      if WindowSize > Length(FTestData) then
        Continue;
        
      for K in KValues do
      begin
        if K > WindowSize then
          Continue;
          
        // Benchmark KthSmallestInSlidingWindow
        FAnalyzer.StartMeasurement;
        Results := SlidingWindow.KthSmallestInSlidingWindow(WindowSize, K);
        Measurement := FAnalyzer.EndMeasurement(
          Format('KthSmallestInSlidingWindow(w=%d,k=%d)', [WindowSize, K]),
          Length(FTestData),
          WindowSize,
          Length(Results),
          'O(n*k*log(k))',
          'O(k)'
        );
        
        WriteLn(Format('  Window=%d, K=%d: %s', [
          WindowSize, K, FAnalyzer.FormatMeasurement(Measurement)
        ]));
        
        // Benchmark single position for comparison
        if Length(FTestData) >= WindowSize then
        begin
          FAnalyzer.StartMeasurement;
          SingleResult := SlidingWindow.KthSmallestAtPosition(0, WindowSize, K);
          Measurement := FAnalyzer.EndMeasurement(
            Format('KthSmallestAtPosition(w=%d,k=%d)', [WindowSize, K]),
            WindowSize,
            0,
            SingleResult,
            'O(k*log(k))',
            'O(k)'
          );
          
          WriteLn(Format('    Single position: %s', [
            FAnalyzer.FormatMeasurement(Measurement)
          ]));
        end;
      end;
    end;
    
    WriteLn('k-th Smallest Element benchmarks completed.');
  finally
    SlidingWindow.Free;
  end;
end;

end.