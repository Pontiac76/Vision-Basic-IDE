unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Clipbrd, Menus, SynHighlighterVB, SynEdit, SynHighlighterAny;

type

  { TForm1 }

  TForm1 = class(TForm)
    IdleTimer1: TIdleTimer;
    MainMenu1: TMainMenu;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SynAnySyn1: TSynAnySyn;
    TheCode: TSynEdit;
    BasicOutput: TSynEdit;
    DeltaBasic: TSynEdit;
    procedure FormCreate (Sender: TObject);
    procedure FormDestroy (Sender: TObject);
    procedure MenuItem3Click (Sender: TObject);
    procedure TheCodeChange (Sender: TObject);
    procedure TheCodeKeyDown (Sender: TObject; var Key: word; Shift: TShiftState);
  private
    LastUpdate: TStringList;
    function CreateLabels (Code: TStringList): string;
    procedure CorrectSource;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

const
  CRLF = chr(13) + chr(10);

function Clamp (inNum, inMin, inMax: integer): integer;
begin
  Result := EnsureRange(inNum, inMin, inMax);
end;

procedure TForm1.FormCreate (Sender: TObject);
var
  AppSize: TRect;
  ActMon: integer;
  Mon: TMonitor;
begin
  ActMon := 1;
  Mon := Screen.Monitors[ActMon];
  AppSize := Mon.BoundsRect;
  AppSize.Width := round(AppSize.Width * 0.95);
  AppSize.Height := round(AppSize.Height * 0.95);
  AppSize.Left := Mon.Left + (Mon.Width div 2 - AppSize.Width div 2);
  AppSize.Top := Mon.Top + (Mon.Height div 2 - AppSize.Height div 2);
  Form1.SetBounds(AppSize.Left, AppSize.Top, AppSize.Width, AppSize.Height);
  LastUpdate := TStringList.Create;
end;

procedure TForm1.FormDestroy (Sender: TObject);
begin
  LastUpdate.Clear;
end;

procedure TForm1.MenuItem3Click (Sender: TObject);
begin
  LastUpdate.Clear;
  TheCode.Clear;
  TheCode.Modified := False;
  BasicOutput.Clear;
  DeltaBasic.Clear;
end;

procedure TForm1.TheCodeChange (Sender: TObject);
var
  BasicCode: TStringList;
  CurrentLine: integer = 0;
  LineMult: integer = 10;
  NextLineJump: integer;
  CommentMode: boolean = False;
  IncludeComments: boolean = False;
  x: integer;
  CurLine: string;
  FirstWord: string;
  NewLine: integer;
  i, r: integer;
  TopLine: integer;
  TempSt: string;
(*
Current Commands
- Beginning with a line with a number changes the current line number
- Using the ^ rounds up to the next multiple set of line numbers.
IE:
:If currently at line 160, entering ^100 takes to you line 200
:If currently at line 150,160,170, entering ^30 takes you to line 180
:Forumla NewLine = CurrentLine - (CurrentLine Mod Multiple) + Multiple
:     [Note the extra math due to current line ACTUALLY being the next line to use]

]Comments=[1/0] - Turn on ; statements (So long ; is the first character on the line)
]LineMult=### - Each line number increases by ### amount
<# and #> - Block comments.  these are always hidden from built code - Internal documentation
. - As the first character, passes this text directly to the command line (Skips numbering the line)
*)
begin
  if TheCode.Modified then begin
    CorrectSource;
    TheCode.Modified := False;
    BasicCode := TStringList.Create;
    for x := 0 to TheCode.Lines.Count - 1 do begin
      CurLine := TheCode.Lines[x].Trim;
      if (CurLine <> '') then begin
        // See if this line contains a new line number
        FirstWord := CurLine.Split(' ')[0];
        val(FirstWord, NewLine, r);
        if (r = 0) then begin
          if (CurrentLine <= NewLine) then begin
            CurrentLine := clamp(NewLine, CurrentLine, 65535);
            CurLine := trim(copy(CurLine, Length(IntToStr(CurrentLine)) + 1, maxlongint));
          end else begin
            // Needed this next line to trim the "bad" line number
            CurLine := trim(copy(CurLine, Length(IntToStr(CurrentLine)) + 1, maxlongint));
            if (LineMult > 2) then begin
              BasicCode.Add('[VBCE-Line ' + (x + 1).ToString + '] Cannot set line number to below current line number.');//  Attempting to set line ' + NewLine.ToString + ' when current line is ' + CurrentLine.ToString);
            end;
          end;
        end;

        // Start digging through the different commands
        if (CurLine.startsWith(']comments=')) then begin
          TempST := CurLine.trim.Split('=')[1];
          IncludeComments := TempSt = '1';
        end else if Pos(']linemult=', CurLine) = 1 then begin
          val(CurLine.Split('=')[1], i, r);
          if ((r = 0) and (i > 0)) then begin
            LineMult := clamp(i, 1, 65535);
          end;
        end else if (CurLine.trim.StartsWith('^')) then begin
          val(CurLine.Substring(1), i, r);
          if r = 0 then begin
            i := clamp(i, 1, 65535);
            r := (CurrentLine - LineMult) mod i + LineMult;
            i := CurrentLine - r + i;
            // i:=i+CurrentLine;
            CurrentLine := i;
          end;
        end else if copy(CurLine.Trim, 0, 1) = '#' then begin
          // NOOP - No other condition will be checked if this above statement is true
          // This comment is STRICTLY for IDE comments
        end else if (copy(CurLine.Trim, 0, 1) = ';') and (IncludeComments = False) then begin
          // NOOP - Only if we're not tossing acceptable comments in
          // This can be used for BASIC internal comments
        end else if (Pos('<#', CurLine) = 1) then begin
          CommentMode := True;
          //      end else if ((Pos('#>', Trim(CurLine)) and (Length(Trim(CurLine)) - 2))) then begin
        end else if (CurLine.Trim.EndsWith('#>')) then begin
          CommentMode := False;
        end else if (Pos('.', Trim(CurLine)) = 1) then begin
          TempSt := TempSt + Copy(Trim(CurLine), 2, Length(Trim(CurLine))) + #10;
        end else begin
          // We've looked at all the custom commands, so now we're going to actively write to the script
          if ((CurLine <> '') and (CommentMode = False)) then begin
            TempSt := trim(IntToStr(currentline) + ' ' + CurLine);
            if (TempSt.length > 79) then begin
              // We've hit the 80 character limit per program line
              // Trim to 79 characters so we have room to hit ENTER on the line
              TempSt := TempSt.substring(0, 79);
              // Add a comment stating the line is too long
              if (linemult > 2) then begin
                TempSt := TempSt + CRLF + IntToStr(currentline + 1) + ' rem previous line too long. expect errors. forced trim to 79 characters';
              end;
              //            console.log('[VBCE-Line ' + (i + 1) + '] String too long for V2 Basic.  Forced to 79 chars')
            end;
            BasicCode.Add(TempSt);
            CurrentLine += linemult;
          end;
        end;
      end;
    end;
    TopLine := BasicOutput.TopLine;
    BasicCode.Text := CreateLabels(BasicCode);
    BasicOutput.Text := BasicCode.Text + CRLF;
    BasicOutput.TopLine := TopLine;
    BasicCode.Free;
  end;
end;


function ConvertStringListToKeyValue (Strings: TStringList): TStringList;
var
  i: integer;
  Line: string;
  SpacePos: integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  Result := sl;
  for i := 0 to Strings.Count - 1 do begin
    Line := Strings[i];
    SpacePos := Pos(' ', Line);
    if SpacePos > 0 then begin
      Line := Copy(Line, 1, SpacePos - 1) + '=' + Copy(Line, SpacePos + 1, Length(Line));
      Result.Add(Line);
    end;
  end;
end;

function CompareListings (LineNumbers: TList; Listing1KeyValue, Listing2KeyValue: TStringList): TStringList;
var
  i: integer;
  LineNumber: integer;
  Line1, Line2: string;
begin
  Result := TStringList.Create;
  for i := 0 to LineNumbers.Count - 1 do begin
    LineNumber := integer(LineNumbers[i]);
    Line1 := Listing1KeyValue.Values[IntToStr(LineNumber)];
    Line2 := Listing2KeyValue.Values[IntToStr(LineNumber)];
    if Line2 = '' then begin
      if Line1 = '' then begin
        Continue;
      end else begin
        Result.Add(IntToStr(LineNumber));
      end;
    end else begin
      if Line1 <> Line2 then begin
        Result.Add(IntToStr(LineNumber) + ' ' + Line2);
      end;
    end;
  end;
end;


procedure ExtractLineNumbers (var inLineNumber: TList; var ProgramListing: TStringList);
var
  i: integer;
  Line: string;
  LineNumberStr: string;
  LineNumber: integer;
begin
  // Extract line numbers from Listing1 and Listing2
  // and put them in a tList for
  for i := 0 to ProgramListing.Count - 1 do begin
    Line := ProgramListing[i];
    // Extract the line number from the current line of code
    LineNumberStr := Copy(Line, 1, Pos(' ', Line) - 1);
    if (LineNumberStr <> '') then begin
      LineNumber := StrToInt(LineNumberStr);
      // Add the line number to the TList if it doesn't exist
      if inLineNumber.IndexOf(Pointer(LineNumber)) = -1 then begin
        inLineNumber.Add(Pointer(LineNumber));
      end;
    end;
  end;
end;

procedure TForm1.TheCodeKeyDown (Sender: TObject; var Key: word; Shift: TShiftState);
var
  Listing1, Listing2: TStringList;
  LineNumbers: TList;
  Listing1KeyValue, Listing2KeyValue: TStringList;
  DiffCode: TStringList;

begin
  if (ssCtrl in Shift) and (Key = 186) then begin // 186=Semicolon
    // Create the containing variables
    TheCode.Modified := True;
    TheCodeChange(nil);
    Listing1 := TStringList.Create;
    Listing2 := TStringList.Create;
    LineNumbers := TList.Create;

    // Populate the variables with what is on screen
    { #todo : Change the following so that it pulls from historical records, not on screen records }
    Listing1.Text := LastUpdate.Text;
    Listing2.Text := BasicOutput.Text;
    LastUpdate.Text := BasicOutput.Text;

    // Get the LineNumbers from both recently generated code and previously generated code.
    ExtractLineNumbers(LineNumbers, Listing1);
    ExtractLineNumbers(LineNumbers, Listing2);

    // Take the two listings and convert the lines to key/value pairs
    Listing1KeyValue := ConvertStringListToKeyValue(Listing1);
    Listing2KeyValue := ConvertStringListToKeyValue(Listing2);

    // Run the difference check between the code
    DiffCode := CompareListings(LineNumbers, Listing1KeyValue, Listing2KeyValue);

    // Put the difference on the UI (There's no saving of this code)
    DeltaBasic.Text := DiffCode.Text;

    // Clen things up
    Listing2KeyValue.Free;
    Listing1KeyValue.Free;
    DiffCode.Free;

    // Set the previous version of code to the currently generated code.
    LastUpdate.Text := BasicOutput.Text;

    { #todo: Instead of keeping tabs on just the previously generated code, an enhancement will be to allow selecting
             any version of previously generated code to do a compare against}
  end;

end;

procedure TForm1.CorrectSource;
var
  x: integer;
  BasicLine: string;
  LabelName: string;
  OL: string; // Original Line
begin
  for x := 0 to TheCode.Lines.Count - 1 do begin
    // Var to toy with
    OL := TheCode.Lines[x].trim;

    // Remove spaces from labels
    // Labels are considered only if they're the FIRST character
    // on the line of the SOURCE code, not COMPILED code
    if (TheCode.Lines[x].trim.StartsWith('@')) and (TheCode.Lines[x].Contains(':')) then begin
      // Obtain the full label name
      LabelName := OL.Split(':')[0];
      // Rip out the spaces
      LabelName := LabelName.Replace(' ', '', [rfReplaceAll]);

      // Reconstruct the command
      BasicLine := LabelName + copy(OL, pos(':', OL), MaxLongint);

      // Drop it in
      TheCode.Lines[x] := BasicLine;
    end;
  end;
end;

function TForm1.CreateLabels (Code: TStringList): string;
var
  x, y: integer;
  LineNum: string;
  BasicLine: string;
  LabelAs: string;
  LabelsFound: TStringList;
  Proceed: boolean = True;
  MaxLenLabel: string;
begin
  // Init Vars
  LabelsFound := TStringList.Create;

  // Clean up source code
  // - Fix up the label declarations - No spaces allowed
  for x := 0 to Code.Count - 1 do begin
    LabelAs := Code[x].Split(' ')[1].Trim;
    if (LabelAs.Substring(0, 1) = '@') then begin
      LabelAs := LabelAs.Split(':')[0].trim;
      BasicLine := copy(code[x], pos(':', code[x]), maxlongint);
      LabelAs := LabelAs.Replace(' ', '', [rfReplaceAll]);
      LineNum := Code[x].Split(' ')[0].trim + ' ' + LabelAs + BasicLine;
      Code[x] := LineNum;
    end;
  end;
  // Find all defined labels
  for x := 0 to Code.Count - 1 do begin
    if Proceed then begin
      LabelAs := Code[x].Split(' ')[1].Trim;
      // If the Label isn't at least two characters, skip the check (Not valid) - @ is 1 char, and then another char
      if (LabelAs.Length >= 2) then begin
        LineNum := Code[x].Split(' ')[0].trim;
        if (LabelAs.Substring(0, 1) = '@') then begin
          LabelAs := LabelAs.Split(':')[0].Trim;
          if (LabelsFound.IndexOfName(LabelAs) = -1) then begin
            LabelsFound.Values[LabelAs] := LineNum;
            if Code[x].Split(':')[1].Trim = '' then begin
              Code[x] := Code[x] + 'rem label:' + LabelAs.ToLower.Substring(1);
            end;
          end else begin
            Proceed := False;
            Code.Clear;
            Code.Add('ERROR: Redclared Labels');
            for y := 0 to TheCode.Lines.Count - 1 do begin
              if (TheCode.Lines[y].StartsWith(LabelAs + ':')) then begin
                code.Add('Line# ' + IntToStr(y + 1) + ': ' + TheCode.Lines[y]);
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  // Now go through and do the changes
  // We need to go from longest keyword down to shortest. (IE: If we have a label @FirstWord and @FirstWo then
  // there's the potential to replace @FirstWord with @FirstWo values.
  while Proceed and (LabelsFound.Count <> 0) do begin
    // Reset the label
    MaxLenLabel := '';

    // Discover the longest length label
    for x := 0 to LabelsFound.Count - 1 do begin
      if (LabelsFound.Names[x].Length > MaxLenLabel.Length) then begin
        MaxLenLabel := LabelsFound.Names[x];
      end;
    end;

    LineNum := LabelsFound.Values[MaxLenLabel];
    // First, pull out the label
    Code.Text := Code.Text.Replace(MaxLenLabel + ':', '', [rfReplaceAll, rfIgnoreCase]);
    // Now do the rest of the code
    code.Text := Code.Text.Replace(MaxLenLabel, LineNum, [rfReplaceAll, rfIgnoreCase]);
    // We're done with this particular key, nuke it
    LabelsFound.Delete(LabelsFound.IndexOfName(MaxLenLabel));
  end;

  // Send out the results
  Result := code.Text;

  // Cleanup
  LabelsFound.Free;

end;

initialization

finalization
end.
