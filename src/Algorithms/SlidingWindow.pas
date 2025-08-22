unit SlidingWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  // Generic sliding window class for different data types
  TSlidingWindow<T> = class
  private
    FData: TArray<T>;
    FWindowSize: Integer;
    FCurrentIndex: Integer;
    FWindow: TQueue<T>;
    FSum: T;
    FOnCompare: TComparison<T>;
    FOnAdd: TFunc<T, T, T>;
    FOnSubtract: TFunc<T, T, T>;
  public
    constructor Create(const AData: TArray<T>; AWindowSize: Integer;
      AOnCompare: TComparison<T>; AOnAdd: TFunc<T, T, T>; AOnSubtract: TFunc<T, T, T>);
    destructor Destroy; override;
    
    // Fixed-size sliding window algorithms
    function MaximumSumSubarray: T;
    function MinimumSumSubarray: T;
    function MaximumElementInWindow: TArray<T>;
    function MinimumElementInWindow: TArray<T>;
    
    // Variable-size sliding window algorithms
    function LongestSubarrayWithSum(TargetSum: T): Integer;
    function ShortestSubarrayWithSum(TargetSum: T): Integer;
    function MaximumLengthSubarrayWithDistinctElements: Integer;
    
    // Utility methods
    procedure Reset;
    function GetCurrentWindow: TArray<T>;
    property WindowSize: Integer read FWindowSize;
    property CurrentIndex: Integer read FCurrentIndex;
  end;

  // Specialized integer sliding window with optimized algorithms
  TIntegerSlidingWindow = class
  private
    FData: TArray<Integer>;
    FSize: Integer;
  public
    constructor Create(const AData: TArray<Integer>);
    
    // Fixed-size window algorithms - O(n) complexity
    function MaxSumSubarray(WindowSize: Integer): Integer;
    function MinSumSubarray(WindowSize: Integer): Integer;
    function MaxProductSubarray(WindowSize: Integer): Int64;
    
    // Variable-size window algorithms
    function LongestSubarrayWithAtMostKDistinct(K: Integer): Integer;
    function SmallestSubarrayWithSumGreaterThan(Target: Integer): Integer;
    function LongestSubarrayWithSumEqualsK(K: Integer): Integer;
    
    // Advanced sliding window algorithms
    function MaximumOfAllSubarraysOfSizeK(WindowSize: Integer): TArray<Integer>;
    function SlidingWindowMaximum(WindowSize: Integer): TArray<Integer>;
    function CountSubarraysWithSumK(K: Integer): Integer;
    
    property Data: TArray<Integer> read FData;
    property Size: Integer read FSize;
  end;

  // String sliding window for substring problems
  TStringSlidingWindow = class
  private
    FText: string;
    FLength: Integer;
  public
    constructor Create(const AText: string);
    
    // String sliding window algorithms
    function LongestSubstringWithoutRepeatingChars: Integer;
    function LongestSubstringWithAtMostKDistinctChars(K: Integer): Integer;
    function MinimumWindowSubstring(const Pattern: string): string;
    function FindAllAnagrams(const Pattern: string): TArray<Integer>;
    function LongestRepeatingCharacterReplacement(const C: Char; K: Integer): Integer;
    
    property Text: string read FText;
    property Length: Integer read FLength;
  end;

implementation

{ TSlidingWindow<T> }

constructor TSlidingWindow<T>.Create(const AData: TArray<T>; AWindowSize: Integer;
  AOnCompare: TComparison<T>; AOnAdd: TFunc<T, T, T>; AOnSubtract: TFunc<T, T, T>);
begin
  inherited Create;
  FData := AData;
  FWindowSize := AWindowSize;
  FCurrentIndex := 0;
  FWindow := TQueue<T>.Create;
  FOnCompare := AOnCompare;
  FOnAdd := AOnAdd;
  FOnSubtract := AOnSubtract;
end;

destructor TSlidingWindow<T>.Destroy;
begin
  FWindow.Free;
  inherited Destroy;
end;

function TSlidingWindow<T>.MaximumSumSubarray: T;
var
  I: Integer;
  CurrentSum, MaxSum: T;
begin
  if Length(FData) < FWindowSize then
    raise Exception.Create('Array size is smaller than window size');
  
  // Calculate sum of first window
  CurrentSum := Default(T);
  for I := 0 to FWindowSize - 1 do
    CurrentSum := FOnAdd(CurrentSum, FData[I]);
  
  MaxSum := CurrentSum;
  
  // Slide the window and update sum
  for I := FWindowSize to High(FData) do
  begin
    CurrentSum := FOnSubtract(CurrentSum, FData[I - FWindowSize]);
    CurrentSum := FOnAdd(CurrentSum, FData[I]);
    if FOnCompare(CurrentSum, MaxSum) > 0 then
      MaxSum := CurrentSum;
  end;
  
  Result := MaxSum;
end;

function TSlidingWindow<T>.MinimumSumSubarray: T;
var
  I: Integer;
  CurrentSum, MinSum: T;
begin
  if Length(FData) < FWindowSize then
    raise Exception.Create('Array size is smaller than window size');
  
  // Calculate sum of first window
  CurrentSum := Default(T);
  for I := 0 to FWindowSize - 1 do
    CurrentSum := FOnAdd(CurrentSum, FData[I]);
  
  MinSum := CurrentSum;
  
  // Slide the window and update sum
  for I := FWindowSize to High(FData) do
  begin
    CurrentSum := FOnSubtract(CurrentSum, FData[I - FWindowSize]);
    CurrentSum := FOnAdd(CurrentSum, FData[I]);
    if FOnCompare(CurrentSum, MinSum) < 0 then
      MinSum := CurrentSum;
  end;
  
  Result := MinSum;
end;

function TSlidingWindow<T>.MaximumElementInWindow: TArray<T>;
var
  I, J: Integer;
  MaxElement: T;
begin
  SetLength(Result, Length(FData) - FWindowSize + 1);
  
  for I := 0 to Length(FData) - FWindowSize do
  begin
    MaxElement := FData[I];
    for J := I + 1 to I + FWindowSize - 1 do
      if FOnCompare(FData[J], MaxElement) > 0 then
        MaxElement := FData[J];
    Result[I] := MaxElement;
  end;
end;

function TSlidingWindow<T>.MinimumElementInWindow: TArray<T>;
var
  I, J: Integer;
  MinElement: T;
begin
  SetLength(Result, Length(FData) - FWindowSize + 1);
  
  for I := 0 to Length(FData) - FWindowSize do
  begin
    MinElement := FData[I];
    for J := I + 1 to I + FWindowSize - 1 do
      if FOnCompare(FData[J], MinElement) < 0 then
        MinElement := FData[J];
    Result[I] := MinElement;
  end;
end;

function TSlidingWindow<T>.LongestSubarrayWithSum(TargetSum: T): Integer;
var
  Left, Right: Integer;
  CurrentSum: T;
  MaxLength: Integer;
begin
  Left := 0;
  Right := 0;
  CurrentSum := Default(T);
  MaxLength := 0;
  
  while Right < Length(FData) do
  begin
    CurrentSum := FOnAdd(CurrentSum, FData[Right]);
    
    while (Left <= Right) and (FOnCompare(CurrentSum, TargetSum) > 0) do
    begin
      CurrentSum := FOnSubtract(CurrentSum, FData[Left]);
      Inc(Left);
    end;
    
    if FOnCompare(CurrentSum, TargetSum) = 0 then
      MaxLength := Max(MaxLength, Right - Left + 1);
    
    Inc(Right);
  end;
  
  Result := MaxLength;
end;

function TSlidingWindow<T>.ShortestSubarrayWithSum(TargetSum: T): Integer;
var
  Left, Right: Integer;
  CurrentSum: T;
  MinLength: Integer;
begin
  Left := 0;
  Right := 0;
  CurrentSum := Default(T);
  MinLength := MaxInt;
  
  while Right < Length(FData) do
  begin
    CurrentSum := FOnAdd(CurrentSum, FData[Right]);
    
    while (Left <= Right) and (FOnCompare(CurrentSum, TargetSum) >= 0) do
    begin
      MinLength := Min(MinLength, Right - Left + 1);
      CurrentSum := FOnSubtract(CurrentSum, FData[Left]);
      Inc(Left);
    end;
    
    Inc(Right);
  end;
  
  if MinLength = MaxInt then
    Result := -1
  else
    Result := MinLength;
end;

function TSlidingWindow<T>.MaximumLengthSubarrayWithDistinctElements: Integer;
// This is a placeholder - implementation would depend on the specific type T
begin
  Result := 0;
  // Implementation would require a way to check for distinct elements
  // which depends on the specific type T
end;

procedure TSlidingWindow<T>.Reset;
begin
  FCurrentIndex := 0;
  FWindow.Clear;
  FSum := Default(T);
end;

function TSlidingWindow<T>.GetCurrentWindow: TArray<T>;
var
  I: Integer;
begin
  SetLength(Result, FWindow.Count);
  I := 0;
  for var Item in FWindow do
  begin
    Result[I] := Item;
    Inc(I);
  end;
end;

{ TIntegerSlidingWindow }

constructor TIntegerSlidingWindow.Create(const AData: TArray<Integer>);
begin
  inherited Create;
  FData := AData;
  FSize := Length(AData);
end;

function TIntegerSlidingWindow.MaxSumSubarray(WindowSize: Integer): Integer;
var
  I: Integer;
  CurrentSum, MaxSum: Integer;
begin
  if FSize < WindowSize then
    raise Exception.Create('Array size is smaller than window size');
  
  // Calculate sum of first window
  CurrentSum := 0;
  for I := 0 to WindowSize - 1 do
    CurrentSum := CurrentSum + FData[I];
  
  MaxSum := CurrentSum;
  
  // Slide the window
  for I := WindowSize to FSize - 1 do
  begin
    CurrentSum := CurrentSum - FData[I - WindowSize] + FData[I];
    MaxSum := Max(MaxSum, CurrentSum);
  end;
  
  Result := MaxSum;
end;

function TIntegerSlidingWindow.MinSumSubarray(WindowSize: Integer): Integer;
var
  I: Integer;
  CurrentSum, MinSum: Integer;
begin
  if FSize < WindowSize then
    raise Exception.Create('Array size is smaller than window size');
  
  // Calculate sum of first window
  CurrentSum := 0;
  for I := 0 to WindowSize - 1 do
    CurrentSum := CurrentSum + FData[I];
  
  MinSum := CurrentSum;
  
  // Slide the window
  for I := WindowSize to FSize - 1 do
  begin
    CurrentSum := CurrentSum - FData[I - WindowSize] + FData[I];
    MinSum := Min(MinSum, CurrentSum);
  end;
  
  Result := MinSum;
end;

function TIntegerSlidingWindow.MaxProductSubarray(WindowSize: Integer): Int64;
var
  I: Integer;
  CurrentProduct, MaxProduct: Int64;
begin
  if FSize < WindowSize then
    raise Exception.Create('Array size is smaller than window size');
  
  // Calculate product of first window
  CurrentProduct := 1;
  for I := 0 to WindowSize - 1 do
    CurrentProduct := CurrentProduct * FData[I];
  
  MaxProduct := CurrentProduct;
  
  // For sliding window with product, we need to handle division
  // This is a simplified version - in practice, you'd need to handle zeros
  for I := WindowSize to FSize - 1 do
  begin
    if FData[I - WindowSize] <> 0 then
      CurrentProduct := CurrentProduct div FData[I - WindowSize];
    CurrentProduct := CurrentProduct * FData[I];
    MaxProduct := Max(MaxProduct, CurrentProduct);
  end;
  
  Result := MaxProduct;
end;

function TIntegerSlidingWindow.LongestSubarrayWithAtMostKDistinct(K: Integer): Integer;
var
  Left, Right: Integer;
  FreqMap: TDictionary<Integer, Integer>;
  MaxLength: Integer;
begin
  FreqMap := TDictionary<Integer, Integer>.Create;
  try
    Left := 0;
    MaxLength := 0;
    
    for Right := 0 to FSize - 1 do
    begin
      // Add current element to frequency map
      if FreqMap.ContainsKey(FData[Right]) then
        FreqMap[FData[Right]] := FreqMap[FData[Right]] + 1
      else
        FreqMap.Add(FData[Right], 1);
      
      // Shrink window if we have more than K distinct elements
      while FreqMap.Count > K do
      begin
        FreqMap[FData[Left]] := FreqMap[FData[Left]] - 1;
        if FreqMap[FData[Left]] = 0 then
          FreqMap.Remove(FData[Left]);
        Inc(Left);
      end;
      
      MaxLength := Max(MaxLength, Right - Left + 1);
    end;
    
    Result := MaxLength;
  finally
    FreqMap.Free;
  end;
end;

function TIntegerSlidingWindow.SmallestSubarrayWithSumGreaterThan(Target: Integer): Integer;
var
  Left, Right: Integer;
  CurrentSum: Integer;
  MinLength: Integer;
begin
  Left := 0;
  CurrentSum := 0;
  MinLength := MaxInt;
  
  for Right := 0 to FSize - 1 do
  begin
    CurrentSum := CurrentSum + FData[Right];
    
    while (Left <= Right) and (CurrentSum > Target) do
    begin
      MinLength := Min(MinLength, Right - Left + 1);
      CurrentSum := CurrentSum - FData[Left];
      Inc(Left);
    end;
  end;
  
  if MinLength = MaxInt then
    Result := -1
  else
    Result := MinLength;
end;

function TIntegerSlidingWindow.LongestSubarrayWithSumEqualsK(K: Integer): Integer;
var
  Left, Right: Integer;
  CurrentSum: Integer;
  MaxLength: Integer;
begin
  Left := 0;
  CurrentSum := 0;
  MaxLength := 0;
  
  for Right := 0 to FSize - 1 do
  begin
    CurrentSum := CurrentSum + FData[Right];
    
    while (Left <= Right) and (CurrentSum > K) do
    begin
      CurrentSum := CurrentSum - FData[Left];
      Inc(Left);
    end;
    
    if CurrentSum = K then
      MaxLength := Max(MaxLength, Right - Left + 1);
  end;
  
  Result := MaxLength;
end;

function TIntegerSlidingWindow.MaximumOfAllSubarraysOfSizeK(WindowSize: Integer): TArray<Integer>;
var
  I, J: Integer;
  MaxElement: Integer;
begin
  SetLength(Result, FSize - WindowSize + 1);
  
  for I := 0 to FSize - WindowSize do
  begin
    MaxElement := FData[I];
    for J := I + 1 to I + WindowSize - 1 do
      MaxElement := Max(MaxElement, FData[J]);
    Result[I] := MaxElement;
  end;
end;

function TIntegerSlidingWindow.SlidingWindowMaximum(WindowSize: Integer): TArray<Integer>;
// Optimized version using deque would be implemented here
// For now, using the simpler approach
begin
  Result := MaximumOfAllSubarraysOfSizeK(WindowSize);
end;

function TIntegerSlidingWindow.CountSubarraysWithSumK(K: Integer): Integer;
var
  I, J: Integer;
  CurrentSum: Integer;
  Count: Integer;
begin
  Count := 0;
  
  for I := 0 to FSize - 1 do
  begin
    CurrentSum := 0;
    for J := I to FSize - 1 do
    begin
      CurrentSum := CurrentSum + FData[J];
      if CurrentSum = K then
        Inc(Count)
      else if CurrentSum > K then
        Break;
    end;
  end;
  
  Result := Count;
end;

{ TStringSlidingWindow }

constructor TStringSlidingWindow.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
  FLength := System.Length(AText);
end;

function TStringSlidingWindow.LongestSubstringWithoutRepeatingChars: Integer;
var
  Left, Right: Integer;
  CharSet: TDictionary<Char, Integer>;
  MaxLength: Integer;
begin
  CharSet := TDictionary<Char, Integer>.Create;
  try
    Left := 1;
    MaxLength := 0;
    
    for Right := 1 to FLength do
    begin
      if CharSet.ContainsKey(FText[Right]) and (CharSet[FText[Right]] >= Left) then
        Left := CharSet[FText[Right]] + 1;
      
      CharSet.AddOrSetValue(FText[Right], Right);
      MaxLength := Max(MaxLength, Right - Left + 1);
    end;
    
    Result := MaxLength;
  finally
    CharSet.Free;
  end;
end;

function TStringSlidingWindow.LongestSubstringWithAtMostKDistinctChars(K: Integer): Integer;
var
  Left, Right: Integer;
  CharCount: TDictionary<Char, Integer>;
  MaxLength: Integer;
begin
  CharCount := TDictionary<Char, Integer>.Create;
  try
    Left := 1;
    MaxLength := 0;
    
    for Right := 1 to FLength do
    begin
      // Add current character
      if CharCount.ContainsKey(FText[Right]) then
        CharCount[FText[Right]] := CharCount[FText[Right]] + 1
      else
        CharCount.Add(FText[Right], 1);
      
      // Shrink window if more than K distinct characters
      while CharCount.Count > K do
      begin
        CharCount[FText[Left]] := CharCount[FText[Left]] - 1;
        if CharCount[FText[Left]] = 0 then
          CharCount.Remove(FText[Left]);
        Inc(Left);
      end;
      
      MaxLength := Max(MaxLength, Right - Left + 1);
    end;
    
    Result := MaxLength;
  finally
    CharCount.Free;
  end;
end;

function TStringSlidingWindow.MinimumWindowSubstring(const Pattern: string): string;
var
  Left, Right: Integer;
  PatternCount, WindowCount: TDictionary<Char, Integer>;
  Required, Formed: Integer;
  MinLength, MinLeft: Integer;
  I: Integer;
begin
  if FLength < System.Length(Pattern) then
  begin
    Result := '';
    Exit;
  end;
  
  PatternCount := TDictionary<Char, Integer>.Create;
  WindowCount := TDictionary<Char, Integer>.Create;
  try
    // Count characters in pattern
    for I := 1 to System.Length(Pattern) do
    begin
      if PatternCount.ContainsKey(Pattern[I]) then
        PatternCount[Pattern[I]] := PatternCount[Pattern[I]] + 1
      else
        PatternCount.Add(Pattern[I], 1);
    end;
    
    Required := PatternCount.Count;
    Formed := 0;
    Left := 1;
    MinLength := MaxInt;
    MinLeft := 1;
    
    for Right := 1 to FLength do
    begin
      // Add character to window
      if WindowCount.ContainsKey(FText[Right]) then
        WindowCount[FText[Right]] := WindowCount[FText[Right]] + 1
      else
        WindowCount.Add(FText[Right], 1);
      
      // Check if current character frequency matches pattern frequency
      if PatternCount.ContainsKey(FText[Right]) and 
         (WindowCount[FText[Right]] = PatternCount[FText[Right]]) then
        Inc(Formed);
      
      // Try to shrink window
      while (Left <= Right) and (Formed = Required) do
      begin
        if Right - Left + 1 < MinLength then
        begin
          MinLength := Right - Left + 1;
          MinLeft := Left;
        end;
        
        // Remove leftmost character
        if WindowCount.ContainsKey(FText[Left]) then
        begin
          WindowCount[FText[Left]] := WindowCount[FText[Left]] - 1;
          if PatternCount.ContainsKey(FText[Left]) and 
             (WindowCount[FText[Left]] < PatternCount[FText[Left]]) then
            Dec(Formed);
        end;
        Inc(Left);
      end;
    end;
    
    if MinLength = MaxInt then
      Result := ''
    else
      Result := Copy(FText, MinLeft, MinLength);
  finally
    PatternCount.Free;
    WindowCount.Free;
  end;
end;

function TStringSlidingWindow.FindAllAnagrams(const Pattern: string): TArray<Integer>;
var
  Left, Right: Integer;
  PatternCount, WindowCount: TDictionary<Char, Integer>;
  ResultList: TList<Integer>;
  I: Integer;
  PatternLen: Integer;
  
  function DictionariesEqual: Boolean;
  var
    Key: Char;
  begin
    Result := True;
    for Key in PatternCount.Keys do
    begin
      if not WindowCount.ContainsKey(Key) or 
         (WindowCount[Key] <> PatternCount[Key]) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
  
begin
  PatternLen := System.Length(Pattern);
  if FLength < PatternLen then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  
  PatternCount := TDictionary<Char, Integer>.Create;
  WindowCount := TDictionary<Char, Integer>.Create;
  ResultList := TList<Integer>.Create;
  try
    // Count characters in pattern
    for I := 1 to PatternLen do
    begin
      if PatternCount.ContainsKey(Pattern[I]) then
        PatternCount[Pattern[I]] := PatternCount[Pattern[I]] + 1
      else
        PatternCount.Add(Pattern[I], 1);
    end;
    
    Left := 1;
    for Right := 1 to FLength do
    begin
      // Add character to window
      if WindowCount.ContainsKey(FText[Right]) then
        WindowCount[FText[Right]] := WindowCount[FText[Right]] + 1
      else
        WindowCount.Add(FText[Right], 1);
      
      // Maintain window size
      if Right - Left + 1 = PatternLen then
      begin
        if DictionariesEqual then
          ResultList.Add(Left - 1); // Convert to 0-based index
        
        // Remove leftmost character
        WindowCount[FText[Left]] := WindowCount[FText[Left]] - 1;
        if WindowCount[FText[Left]] = 0 then
          WindowCount.Remove(FText[Left]);
        Inc(Left);
      end;
    end;
    
    Result := ResultList.ToArray;
  finally
    PatternCount.Free;
    WindowCount.Free;
    ResultList.Free;
  end;
end;

function TStringSlidingWindow.LongestRepeatingCharacterReplacement(const C: Char; K: Integer): Integer;
var
  Left, Right: Integer;
  CharCount: TDictionary<Char, Integer>;
  MaxCount, MaxLength: Integer;
  
  function GetMaxFrequency: Integer;
  var
    Value: Integer;
  begin
    Result := 0;
    for Value in CharCount.Values do
      Result := Max(Result, Value);
  end;
  
begin
  CharCount := TDictionary<Char, Integer>.Create;
  try
    Left := 1;
    MaxCount := 0;
    MaxLength := 0;
    
    for Right := 1 to FLength do
    begin
      // Add current character
      if CharCount.ContainsKey(FText[Right]) then
        CharCount[FText[Right]] := CharCount[FText[Right]] + 1
      else
        CharCount.Add(FText[Right], 1);
      
      MaxCount := Max(MaxCount, CharCount[FText[Right]]);
      
      // If window size - max frequency > K, shrink window
      while (Right - Left + 1 - MaxCount) > K do
      begin
        CharCount[FText[Left]] := CharCount[FText[Left]] - 1;
        if CharCount[FText[Left]] = 0 then
          CharCount.Remove(FText[Left]);
        Inc(Left);
        MaxCount := GetMaxFrequency;
      end;
      
      MaxLength := Max(MaxLength, Right - Left + 1);
    end;
    
    Result := MaxLength;
  finally
    CharCount.Free;
  end;
end;

end.