unit MainForm;

{$mode objfpc}{$H+}
{$WARN 4056 off : Conversion between ordinals and pointers is not portable}
{$WARN 4046 off : Constructing a class "$1" with abstract method "$2"}
interface

uses
  Classes, SysUtils, Math, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Clipbrd, Menus, SimpleSQLite3, SynHighlighterVB, SynEdit, SynHighlighterAny, shlobj, SynEditTypes, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    IdleTimer1: TIdleTimer;
    MainMenu1: TMainMenu;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem2: TMenuItem;
    mnuNew: TMenuItem;
    mnuOpen: TMenuItem;
    mnuSave: TMenuItem;
    mnuSaveAs: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SynAnySyn1: TSynAnySyn;
    TheCode: TSynEdit;
    BasicOutput: TSynEdit;
    DeltaBasic: TSynEdit;



      procedure BasicOutputMouseUp(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
    procedure FormCreate (Sender: TObject);
    procedure FormDestroy (Sender: TObject);
    procedure MenuItem11Click (Sender: TObject);
    procedure MenuItem2Click (Sender: TObject);
    procedure mnuNewClick (Sender: TObject);
    procedure mnuOpenClick (Sender: TObject);
    procedure mnuSaveClick (Sender: TObject);
    procedure mnuSaveAsClick (Sender: TObject);
    procedure TheCodeChange (Sender: TObject);
    procedure TheCodeKeyDown (Sender: TObject; var Key: word; Shift: TShiftState);
    procedure TheCodeKeyPress (Sender: TObject; var Key: char);

    procedure TheCodeMouseUp (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);

    procedure TheCodeMouseWheel (Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
  private
    CodeVersions: TStringList;
    LastUpdate: TStringList;
    GeneratedFrom: TStringList;
    function CreateLabels (Code: TStringList): string;
    procedure CorrectSource;
    procedure FixBASICCase;
    procedure UpdateBasicOutputPosition;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

uses SQLite3Conn, SQLDB;

const
  CRLF = chr(13) + chr(10);

var
  db: TSQLite3Connection;

function Clamp (inNum, inMin, inMax: integer): integer;
begin
  Result := EnsureRange(inNum, inMin, inMax);
end;

procedure CreateSrcVersionTable;
begin

  DB.ExecuteDirect('drop table if exists SrcVersion');
  DB.ExecuteDirect('CREATE TABLE SrcVersion (id INTEGER PRIMARY KEY AUTOINCREMENT,created_at DATETIME NOT NULL,source_code TEXT NOT NULL);');
  DB.Transaction.Commit;
end;

procedure StoreSrcHist (SrcHist: TStringList);
var
  i: integer;
  Query: TSQLQuery;
  FormattedDateTime: string;
  SourceCode: TStringList;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.SQLConnection := db;
    Query.Transaction := DB.Transaction;
    CreateSrcVersionTable;
    for i := 0 to SrcHist.Count - 1 do begin
      FormattedDateTime := SrcHist[i];
      SourceCode := TStringList(SrcHist.Objects[i]);
      Query.SQL.Text := 'INSERT INTO SrcVersion (created_at, source_code) VALUES (:created_at, :source_code);';
      Query.Params.ParamByName('created_at').AsString := FormattedDateTime;
      Query.Params.ParamByName('source_code').AsString := SourceCode.Text;
      Query.ExecSQL;
    end;
  finally
    DB.Transaction.Commit;
    Query.Free;
  end;
end;

procedure LoadSrcHist (var SrcHist: TStringList);
var
  i: integer;
  Query: TSQLQuery;
  SourceCode: TStringList;
begin
  for i := 0 to SrcHist.Count - 1 do begin
    SourceCode := TStringList(SrcHist.Objects[i]);
    SourceCode.Free;
  end;
  SrcHist.Clear;
  Query := TSQLQuery.Create(nil);
  try
    Query.SQLConnection := db;
    Query.Transaction := DB.Transaction;
    Query.SQL.Text := 'SELECT created_at, source_code FROM SrcVersion;';
    Query.Open;
    while not Query.EOF do begin
      SourceCode := TStringList.Create;
      SourceCode.Text := Query.FieldByName('source_code').AsString;
      SrcHist.AddObject(Query.FieldByName('created_at').AsString, SourceCode);
      Query.Next;
    end;
  finally
    DB.Transaction.Commit;
    Query.Free;
  end;
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
  CodeVersions := TStringList.Create;

  //OpenDB(':memory:', db);
  OpenDB('r:\rodmover.vbas', db);
  CreateSrcVersionTable;
  GeneratedFrom:=TStringList.Create;
end;

procedure TForm1.BasicOutputMouseUp(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
var
    vs:string;
    vi:integer;
    r:integer;
    cy:integer;
    gv:string;
begin
  cy:=BasicOutput.CaretY;
  vs:=inttostr(cy);
  gv:=GeneratedFrom.Names[cy];
  val(gv,vi,r);
  if (vi>0)and(r=0) then
    TheCode.CaretY:=vi;
end;

procedure TForm1.FormDestroy (Sender: TObject);
var
  x: integer;
begin
  LastUpdate.Free;
  for x := 0 to CodeVersions.Count - 1 do begin
    TStringList(CodeVersions.Objects[x]).Free;
  end;
  CodeVersions.Free;

  CloseDB(db);
end;

procedure TForm1.MenuItem11Click (Sender: TObject);
begin
  Form1.Close;
end;

procedure TForm1.MenuItem2Click (Sender: TObject);
begin
  mnuSave.Enabled := DB.DatabaseName <> ':memory:';
end;

procedure TForm1.mnuNewClick (Sender: TObject);
begin
  LastUpdate.Clear;
  TheCode.Clear;
  TheCode.Modified := False;
  BasicOutput.Clear;
  DeltaBasic.Clear;
  CloseDB(db);
  OpenDB(':memory:', db);
end;

procedure TForm1.mnuOpenClick (Sender: TObject);
var
  semi: word;
begin
  if OpenDialog1.Execute then begin
    OpenDialog1.InitialDir := ExtractFilePath(OpenDialog1.FileName);
    CloseDB(db);
    OpenDB(OpenDialog1.FileName, db);
    LoadSrcHist(CodeVersions);
    if (CodeVersions.Count > 0) then begin
      TheCode.Text := TStringList(CodeVersions.Objects[CodeVersions.Count - 1]).Text;
    end else begin
      TheCode.Clear;
    end;
    semi := 186;
    CorrectSource;
    TheCodeKeyDown(nil, semi, [ssCtrl]);
  end;
end;

procedure TForm1.mnuSaveClick (Sender: TObject);
var
  BasicVersion: TStringList;
begin
  BasicVersion := TStringList.Create;
  BasicVersion.Text := TheCode.Text;
  CodeVersions.AddObject(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), TObject(BasicVersion));
  StoreSrcHist(CodeVersions);
  TheCode.Modified := False;
end;

procedure TForm1.mnuSaveAsClick (Sender: TObject);
begin
  if SaveDialog1.Execute then begin
    SaveDialog1.InitialDir := ExtractFilePath(SaveDialog1.FileName);
    CloseDB(db);
    OpenDB(SaveDialog1.FileName, db);
    mnuSaveClick(nil);
  end;
end;


procedure TForm1.TheCodeChange (Sender: TObject);
var
  BasicCode: TStringList;
  CurrentLine: integer = 0;
  LineMult: integer = 10;
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
    GeneratedFrom.Clear;
    TheCode.Modified := False;
    BasicCode := TStringList.Create;
    for x := 0 to TheCode.Lines.Count - 1 do begin
      CurLine := TheCode.Lines[x].Trim;
      if (CurLine <> '') then begin
        // See if this line contains a new line number
        FirstWord := CurLine.Split(' ')[0];
        val(FirstWord, NewLine, r);
        if (r = 0) then begin
          if (CurrentLine - LineMult + 1 <= NewLine) then begin
            CurrentLine := clamp(NewLine, CurrentLine - LineMult + 1, 65535);
            CurLine := trim(copy(CurLine, Length(IntToStr(CurrentLine)) + 1, maxlongint));
          end else begin
            // Needed this next line to trim the "bad" line number
            CurLine := trim(copy(CurLine, Length(IntToStr(CurrentLine)) + 1, maxlongint));
            if (LineMult > 2) then begin
              //              BasicCode.Add('[VBCE-Line ' + (x + 1).ToString + '] Cannot set line number to below current line number.');//  Attempting to set line ' + NewLine.ToString + ' when current line is ' + CurrentLine.ToString);
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
            end;
            BasicCode.Add(TempSt);
            GeneratedFrom.Values[inttostr(x)]:=inttostr(BasicCode.Count-1);
            CurrentLine += linemult;
          end;
        end;
      end;
    end;
    TopLine := BasicOutput.TopLine;
    BasicCode.Text := CreateLabels(BasicCode);
    BasicOutput.Text := BasicCode.Text + CRLF;
    FixBASICCase;
    BasicOutput.TopLine := TopLine;
    BasicCode.Free;
  end;
  UpdateBasicOutputPosition;
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

function SortLineNumbers (Item1, Item2: Pointer): integer;
begin
  if integer(Item1) < integer(Item2) then begin
    Result := -1;
  end else if integer(Item1) > integer(Item2) then begin
    Result := 1;
  end else begin
    Result := 0;
  end;
end;

function CompareListings (LineNumbers: TList; Listing1KeyValue, Listing2KeyValue: TStringList): TStringList;
var
  i: integer;
  LineNumber: integer;
  Line1, Line2: string;
begin
  Result := TStringList.Create;
  LineNumbers.Sort(@SortLineNumbers);
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

function RepeatChar (c: string; Num: integer): string;
var
  x: integer;
  s: string;
begin
  s := '';
  for x := 1 to Num do begin
    s := s + c;
  end;
  Result := s;
end;

procedure TForm1.TheCodeKeyDown (Sender: TObject; var Key: word; Shift: TShiftState);
var
  Listing1, Listing2: TStringList;
  LineNumbers: TList;
  Listing1KeyValue, Listing2KeyValue: TStringList;
  DiffCode: TStringList;
  ldb: TSQLite3Connection;
  ltr: TSQLTransaction;
  lqu: TSQLQuery;
begin
  if (ssAlt in Shift) and (not (ssCtrl in Shift)) and (Key = 186) then begin
    TheCode.Lines.Insert(TheCode.CaretY - 1, ';' + RepeatChar('·', 40 - 7));
    TheCode.Carety := TheCode.CaretY + 1;
    TheCode.CaretX := 0;
  end;
  if (ssCtrl in Shift) and (not (ssAlt in Shift)) and (Key = 186) then begin // 186=Semicolon
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
    DiffCode.Add('');

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


procedure TForm1.FixBASICCase;
var
  x, y: integer;
  OrigLine: string;
  InQuotes: boolean;
  CurTop: integer;

begin
  // Go through and clean up the labels
  BasicOutput.Lines.BeginUpdate; // Take care of screen jitter if we're moving screen content around
  CurTop := BasicOutput.TopLine;   // Since we're manipulating content, need to remember where the top of the editor is that's showing

  // Need to take into consideration the labels as well, so use the above to put the casing back
  // Go through and convert basic code to lower case, taking into consideration quotes.
  for x := 0 to BasicOutput.Lines.Count - 1 do begin
    OrigLine := BasicOutput.Lines[x].Trim;
    if OrigLine.Contains('"') then begin // Special considerations for lines with quotes and comments
      InQuotes := False;
      for y := 1 to Length(OrigLine) do begin
        if not InQuotes then begin
          OrigLine[y] := LowerCase(OrigLine[y]);
        end;
        if OrigLine[y] = '"' then begin
          InQuotes := not InQuotes;
        end;
      end;
    end else begin
      // No quotes?  Convert it to lower case
      OrigLine := OrigLine.ToLower;
    end;
    // Replace the code with the lower case
    BasicOutput.Lines[x] := OrigLine;
  end;

  BasicOutput.TopLine := CurTop; // Reposition the top line
  if y > 0 then begin
    BasicOutput.SelStart := y;
  end;     // Put the carret back in its place
  BasicOutput.Lines.EndUpdate; // Let the refresh happen at its leisure
end;

procedure TForm1.UpdateBasicOutputPosition;
var
  PercCode: real;

begin
  if (TheCode.Focused) then begin
    // How far down are we scrolled down in TheCode window?
    PercCode := TheCode.TopLine / TheCode.Lines.Count;

    // Set the top position of BasicOutput based on that
    BasicOutput.TopLine := floor(BasicOutput.Lines.Count * PercCode);
  end;

end;

procedure TForm1.CorrectSource;
var
  x: integer;
  BasicLine: string;
  LabelName: string;
  OrigLine: string; // Original Line
begin
  // This routine removes any spaces from all labels between @ and :
  // if the first character starts with an @ and there is a : on the line.
  // We only make changes to the label definition.
  // If the user puts a label in wrong later on, that's on them.
  for x := 0 to TheCode.Lines.Count - 1 do begin
    // Var to toy with
    OrigLine := TheCode.Lines[x].trim;

    // Remove spaces from labels
    // Labels are considered only if they're the FIRST character
    // on the line of the SOURCE code, not COMPILED code
    if (TheCode.Lines[x].trim.StartsWith('@')) and (TheCode.Lines[x].Contains(':')) then begin
      // Obtain the full label name
      LabelName := OrigLine.Split(':')[0];

      // Rip out the spaces
      LabelName := LabelName.Replace(' ', '', [rfReplaceAll]);

      // Reconstruct the command
      BasicLine := LabelName + copy(OrigLine, pos(':', OrigLine), MaxLongint);

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

procedure TForm1.TheCodeKeyPress (Sender: TObject; var Key: char);
begin
  UpdateBasicOutputPosition;
end;

procedure TForm1.TheCodeMouseUp (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  UpdateBasicOutputPosition;
end;

procedure TForm1.TheCodeMouseWheel (Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  UpdateBasicOutputPosition;
end;

initialization

finalization
end.
