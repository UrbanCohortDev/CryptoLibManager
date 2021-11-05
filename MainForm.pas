unit MainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  System.Actions,
  Vcl.ActnList,
  Vcl.StdActns,
  Vcl.Buttons,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Grids,
  Vcl.ValEdit,
  Vcl.CheckLst,
  UC.Delphi.Versions,
  Vcl.ExtCtrls;

type

  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    SrcMasterDir: TEdit;
    Label2: TLabel;
    SpeedButton1: TSpeedButton;
    ActionList1: TActionList;
    BrowseForSource: TBrowseForFolder;
    BrowseForTarget: TBrowseForFolder;
    SaveAction: TAction;
    Button1: TButton;
    SpeedButton3: TSpeedButton;
    IgnoreFolderAction: TAction;
    Memo1: TMemo;
    ExecAction: TAction;
    SrcFolders: TCheckListBox;
    Memo2: TMemo;
    Label4: TLabel;
    NotesAction: TAction;
    SingleFolderGroup: TGroupBox;
    Label3: TLabel;
    TgtMasterDir: TEdit;
    SpeedButton2: TSpeedButton;
    SpeedButton4: TSpeedButton;
    GlobalMap: TCheckBox;
    MappingOptionsGroup: TGroupBox;
    Platforms: TCheckListBox;
    DelphiVersion: TComboBox;
    MapPathsButton: TButton;
    BackUpFolder: TEdit;
    Label5: TLabel;
    BrowseForBackupFolder: TBrowseForFolder;
    Button2: TButton;
    MapTest: TCheckBox;
    Button3: TButton;
    CreateMapAction: TAction;
    GroupBox1: TGroupBox;
    Panel1: TPanel;
    Panel2: TPanel;
    PCOptions: TPageControl;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure BrowseForBackupFolderAccept(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BrowseForSourceAccept(Sender: TObject);
    procedure BrowseForTargetAccept(Sender: TObject);
    procedure CreateMapActionExecute(Sender: TObject);
    procedure DelphiVersionChange(Sender: TObject);
    procedure GlobalMapClick(Sender: TObject);
    procedure ExecActionExecute(Sender: TObject);
    procedure IgnoreFolderActionExecute(Sender: TObject);
    procedure MapPathsButtonClick(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
  private
    { Private declarations }
    FIgnoreFolders: TStringList;
    FDelphiVersions: TDelphiVersions;
    FMapPaths: TStrings;
    procedure LoadDelphiVersions;
    procedure LoadSubFolders;
    procedure ProcessTopFolder(AFolder: string);
    procedure Log(const Value: string);
    procedure MapTopFolder(AFolder: string);
    procedure ProcessSourceFolder(SourceFolder, TargetFolder: string);
    procedure CreatePathMap;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.IOUtils,
  System.Types,
  System.IniFiles,
  System.StrUtils,
  System.Win.Registry;

procedure TForm1.BrowseForBackupFolderAccept(Sender: TObject);
begin
  BackUpFolder.Text := BrowseForBackupFolder.Folder;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FDelphiVersions.Free;
  FIgnoreFolders.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  lIni: TIniFile;
begin
  PCOptions.Pages[0].TabVisible := False;
  PCOptions.Pages[1].TabVisible := False;
  PCOptions.ActivePageIndex := 0;
  FDelphiVersions := TDelphiVersions.Create;
  LoadDelphiVersions;
  FIgnoreFolders := TStringList.Create;
  FIgnoreFolders.Sorted := True;
  FIgnoreFolders.Duplicates := dupIgnore;
  FIgnoreFolders.Delimiter := ';';
  lIni := TIniFile.Create(TPath.ChangeExtension(Application.ExeName, '.ini'));
  try
    FIgnoreFolders.DelimitedText := lIni.ReadString('Folders', 'Ignore', '');
    SrcMasterDir.Text := lIni.ReadString('Folders', 'MasterSource', '');
    TgtMasterDir.Text := lIni.ReadString('Folders', 'MasterTarget', '');
    GlobalMap.Checked := lIni.ReadBool('Settings', 'GlobalMap', False);
    BackUpFolder.Text := lIni.ReadString('Mapping', 'BackUpFolder', '');
  finally
    lIni.Free;
  end;

  LoadSubFolders;
end;

procedure TForm1.BrowseForSourceAccept(Sender: TObject);
begin
  SrcMasterDir.Text := BrowseForSource.Folder;
  LoadSubFolders;
end;

procedure TForm1.BrowseForTargetAccept(Sender: TObject);
begin
  TgtMasterDir.Text := BrowseForTarget.Folder;
end;

procedure TForm1.CreateMapActionExecute(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
  CreatePathMap;
  Memo2.Lines.AddStrings(FMapPaths);
  FMapPaths.Free;
  FMapPaths := nil;
end;

procedure TForm1.CreatePathMap;
var
  J: Integer;
begin
  Log('Creating Paths List');
  FMapPaths := TStringList.Create;
  for J := 0 to SrcFolders.Count - 1 do
  begin

    if SrcFolders.Checked[J] then
      MapTopFolder(SrcFolders.Items[J]);
  end;

end;

procedure TForm1.DelphiVersionChange(Sender: TObject);
begin
  Platforms.Items.Clear;
  FDelphiVersions.SupportedPlatformsToStrings(DelphiVersion.Text,
    Platforms.Items);
end;

procedure TForm1.GlobalMapClick(Sender: TObject);
begin
  if GlobalMap.Checked then
  begin
    PCOptions.ActivePageIndex := 1;
    Label4.Caption := 'Test Output';
  end
  else
  begin
    PCOptions.ActivePageIndex := 0;
    Label4.Caption := 'Files that failed to copy to target folder';
  end;
end;

procedure TForm1.ExecActionExecute(Sender: TObject);
var
  I: Integer;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
  for I := 0 to SrcFolders.Count - 1 do
  begin
    if SrcFolders.Checked[I] then
      ProcessTopFolder(SrcFolders.Items[I]);
  end;
  ShowMessage('Complete');
end;

procedure TForm1.IgnoreFolderActionExecute(Sender: TObject);
var
  I: Integer;
  lDir: string;
begin
  I := SrcFolders.ItemIndex;
  if I < 0 then
    Exit;
  lDir := TPath.GetFileName(SrcFolders.Items[I]);
  FIgnoreFolders.Add(lDir);
  SrcFolders.Items.Delete(I);
end;

procedure TForm1.LoadDelphiVersions;
var
  I: Integer;
begin
  for I := 0 to FDelphiVersions.AvailableCount - 1 do
  begin
    DelphiVersion.Items.Add(FDelphiVersions.AvailableVersionName[I]);
  end;
end;

procedure TForm1.LoadSubFolders;
var
  lDirs: TStringDynArray;
  I, X: Integer;
  lDir: string;
begin
  SrcFolders.Items.Clear;
  if SrcMasterDir.Text = '' then
    Exit;
  lDirs := TDirectory.GetDirectories(SrcMasterDir.Text);

  for I := 0 to Length(lDirs) - 1 do
  begin
    lDir := TPath.GetFileName(lDirs[I]);
    X := SrcFolders.Items.Add(lDirs[I]);
    if FIgnoreFolders.IndexOf(lDir) < 0 then
      SrcFolders.Checked[X] := True;
  end;

  SrcFolders.ItemIndex := 0;
end;

procedure TForm1.Log(const Value: string);
begin
  Memo1.Lines.Add(Value);
  Application.ProcessMessages;
end;

procedure TForm1.MapPathsButtonClick(Sender: TObject);
var
  J, I: Integer;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
  CreatePathMap;
  try
    for I := 0 to Platforms.Count - 1 do
    begin
      if Platforms.Checked[I] then
      begin
        Log('Mapping for ' + Platforms.Items[I] + '; Paths Count: ' +
          FMapPaths.Count.ToString);
        FDelphiVersions.OpenLibrary(DelphiVersion.Text, Platforms.Items[I],
          BackUpFolder.Text);

        for J := 0 to FMapPaths.Count - 1 do
        begin

          if FDelphiVersions.AddToLibrary(FMapPaths[J]) then
            Log('Mapped ' + FMapPaths[J])
          else
            Log('Already Exists: ' + FMapPaths[J]);

        end;
        if MapTest.Checked then
        begin
          Memo2.Lines.Add(FMapPaths[I]);
          Memo2.Lines.Add(StringOfChar('-', FMapPaths[I].Length));
          FDelphiVersions.AppendPaths(Memo2.Lines);
          Memo2.Lines.Add(' ');
        end
        else
          FDelphiVersions.SaveAndCloseLibrary;
      end;
    end;
  finally
    FMapPaths.Free;
  end;

  if MapTest.Checked then
    ShowMessage
      ('Complete. Paths are shown in the second memo box. Delphi Paths have not been updated')
  else
    ShowMessage('Complete. Paths have been added to the Delphi Global Paths');
end;

procedure TForm1.MapTopFolder(AFolder: string);

  procedure MapSourceFolder(BFolder: string);
  var
    BFolders: TStringDynArray;
    I: Integer;
  begin

    FMapPaths.Add(BFolder);
    Log('Added: ' + BFolder);
    BFolders := TDirectory.GetDirectories(BFolder);
    for I := 0 to Length(BFolders) - 1 do
      MapSourceFolder(BFolders[I]);

  end;

var
  lFolders: TStringDynArray;
  I: Integer;
  SFolder: string;
begin
  Log('Processing: ' + AFolder);
  lFolders := TDirectory.GetDirectories(AFolder);
  for I := 0 to Length(lFolders) - 1 do
  begin
    if lFolders[I].Contains('.') then
      Continue;
    SFolder := TPath.Combine(lFolders[I], 'Src');
    if TDirectory.Exists(SFolder) then
      Break
    else if True then
    begin
      SFolder := TPath.Combine(lFolders[I], 'Source');
      if TDirectory.Exists(SFolder) then
        Break
    end
    else
      SFolder := '';
  end;

  if SFolder <> '' then
    MapSourceFolder(SFolder)
  else
    Log('Source Folder not Found');
end;

procedure TForm1.ProcessSourceFolder(SourceFolder, TargetFolder: string);

  procedure ProcessThisFolder(const AFolder: string; const ALevel: Integer);
  var
    lFiles, lFolders: TStringDynArray;
    S, lName, lTargetName, fileContents, lInc: string;
  begin
    Log(' ' + StringOfChar('-', ALevel * 2) + ' Processing ' + AFolder);

    lFolders := TDirectory.GetDirectories(AFolder);
    lFiles := TDirectory.GetFiles(AFolder);
    for S in lFiles do
    begin
      lName := TPath.GetFileName(S);
      lTargetName := TPath.Combine(TargetFolder, lName);
      if TPath.GetExtension(S).ToLower = '.pas' then
      begin
        fileContents := TFile.ReadAllText(S);
        lInc := '{$I ' + DupeString('..\', ALevel) + 'Include\';
        fileContents := fileContents.Replace(lInc, '{$I ', [rfReplaceAll]);
        TFile.WriteAllText(lTargetName, fileContents);
        if not TFile.Exists(lTargetName) then
          Memo2.Lines.Add(S)
      end
      else
      begin
        TFile.Copy(S, lTargetName);
        if not TFile.Exists(lTargetName) then
          Memo2.Lines.Add(S)
      end;
    end;

    for S in lFolders do
      ProcessThisFolder(S, ALevel + 1);

  end;

var
  lFolders: TStringDynArray;
  I: Integer;
begin

  lFolders := TDirectory.GetDirectories(SourceFolder);
  for I := 0 to Length(lFolders) - 1 do
    ProcessThisFolder(lFolders[I], 1);
end;

procedure TForm1.ProcessTopFolder(AFolder: string);
  procedure EmptyFolder(const XFolder: string);
  var
    uFiles: TStringDynArray;
    S: string;
  begin
    uFiles := TDirectory.GetFiles(XFolder);
    for S in uFiles do
      TFile.Delete(S);
  end;

var
  TargetFolder, SFolder, lPath, S: string;
  lFiles, lFolders, lSFolders: TStringDynArray;
  I, J: Integer;
begin
  Log('Starting ' + AFolder);
  TargetFolder := TPath.Combine(TgtMasterDir.Text, TPath.GetFileName(AFolder));
  if TDirectory.Exists(TargetFolder) then
    EmptyFolder(TargetFolder)
  else
    TDirectory.CreateDirectory(TargetFolder);
  lFiles := TDirectory.GetFiles(AFolder);
  for I := 0 to Length(lFiles) - 1 do
  begin
    if SameText(TPath.GetFileName(lFiles[I]), 'README.md') then
      TFile.Copy(lFiles[I], TPath.Combine(TargetFolder, 'README.md'))
    else if SameText(TPath.GetFileName(lFiles[I]), 'LICENSE') then
    begin
      lPath := TPath.Combine(TargetFolder, 'LICENSE') + '.txt';
      TFile.Copy(lFiles[I], lPath);
    end;
  end;

  lFolders := TDirectory.GetDirectories(AFolder);
  for I := 0 to Length(lFolders) - 1 do
  begin
    if lFolders[I].Contains('.') then
      Continue;
    SFolder := TPath.Combine(lFolders[I], 'Src');
    if TDirectory.Exists(SFolder) then
      Break
    else
      SFolder := '';
  end;

  if SFolder <> '' then
    ProcessSourceFolder(SFolder, TargetFolder);

end;

procedure TForm1.SaveActionExecute(Sender: TObject);
var
  lIni: TIniFile;
begin
  lIni := TIniFile.Create(TPath.ChangeExtension(Application.ExeName, '.ini'));
  try
    lIni.WriteString('Folders', 'Ignore', FIgnoreFolders.DelimitedText);
    lIni.WriteString('Folders', 'MasterSource', SrcMasterDir.Text);
    lIni.WriteString('Folders', 'MasterTarget', TgtMasterDir.Text);
    lIni.WriteBool('Settings', 'GlobalMap', GlobalMap.Checked);
    lIni.WriteString('Mapping', 'BackUpFolder', BackUpFolder.Text);
  finally
    lIni.Free;
  end;
end;

end.
