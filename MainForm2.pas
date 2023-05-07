unit MainForm2;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  System.Actions,
  Vcl.ActnList,
  Vcl.StdActns,
  System.ImageList,
  Vcl.ImgList,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  DosCommand,
  UC.Delphi.Versions,
  Vcl.CheckLst;

type
  TMainFormNew = class(TForm)
    GitPathLabel: TLabel;
    ActionList1: TActionList;
    FindGit: TFileOpen;
    CryptoLibRootLabel: TLabel;
    DosCommand: TDosCommand;
    CloneUpdateAction: TAction;
    BrowseCryptoLibRoot: TBrowseForFolder;
    SaveSettingsAction: TAction;
    LogMemo: TMemo;
    GitPath: TEdit;
    GitPathButton: TButton;
    CryptoLibsRoot: TEdit;
    CryptoLibRootButton: TButton;
    InstallMethodLabel: TLabel;
    InstallMethod: TComboBox;
    CombinedLibraryPathLabel: TLabel;
    CombinedLibraryPath: TEdit;
    CombinedPathCopyButton: TButton;
    CombinedPathCopyAction: TAction;
    InstallForGroup: TCheckListBox;
    InstallForLabel: TLabel;
    Platforms: TCheckListBox;
    PlatformsLabel: TLabel;
    BrowseForCombinedFolder: TBrowseForFolder;
    Button2: TButton;
    Panel1: TPanel;
    Button1: TButton;
    SaveSettingsButton: TButton;
    CloseAppButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure GitPathChange(Sender: TObject);
    procedure BrowseCryptoLibRootAccept(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CloneUpdateActionExecute(Sender: TObject);
    procedure DosCommandTerminated(Sender: TObject);
    procedure FindGitAccept(Sender: TObject);
    procedure SaveSettingsActionExecute(Sender: TObject);
    procedure CloseAppButtonClick(Sender: TObject);
    procedure CombinedPathCopyActionExecute(Sender: TObject);
    procedure InstallMethodChange(Sender: TObject);
    procedure CryptoLibsRootChange(Sender: TObject);
    procedure InstallForGroupClickCheck(Sender: TObject);
    procedure PlatformsClickCheck(Sender: TObject);
    procedure BrowseForCombinedFolderAccept(Sender: TObject);
    procedure CombinedLibraryPathChange(Sender: TObject);
  private
    { Private declarations }
    FSettingsChanged: Boolean;
    FCommandList: TStrings;
    FDelphiVersions: TDelphiVersions;
    procedure LoadFromRegistry;
    procedure SaveToRegistry;
    procedure SetSettingsChanged(const Value: Boolean);
    procedure LogThis(const Value: String);
    function GetCryptoLibRootPath: String;
    procedure CloneOrUpdate(const LocalPath, GitPath: String);
    function GitCommand: string;
    procedure RunNextCommand;
    procedure InstallToDelphi;
    function CombineLibs: string;
    procedure AddAllPathsToLibrary;
    function GetSelectedVersions: TStringDynarray;
    function GetSelectedPlatforms: TStringDynarray;
  public
    { Public declarations }
    property SettingsChanged: Boolean read FSettingsChanged
      write SetSettingsChanged;
    property CryptoLibRootPath: String read GetCryptoLibRootPath;
  end;

var
  MainFormNew: TMainFormNew;

implementation

{$R *.dfm}

uses System.Win.Registry,
  System.IOUtils,
  System.UITypes,
  ClipBrd,
  CryptoLibProcessor;

const
  REGISTRY_KEY = 'Software\UrbanCohort\DelphiCryptoLibs\';

  HASH_LIB_GIT = 'https://github.com/Xor-el/HashLib4Pascal.git';
  CRYPTO_LIB_GIT = 'https://github.com/Xor-el/CryptoLib4Pascal.git';
  SIMPLE_BASE_LIB_GIT = 'https://github.com/Xor-el/SimpleBaseLib4Pascal.git';
  QR_CODE_GIT = 'https://github.com/Xor-el/QRCodeGenLib4Pascal.git';

  HASH_LIB_PATH = 'HashLib4Pascal';
  CRYPTO_LIB_PATH = 'CryptoLib4Pascal';
  SIMPLE_BASE_LIB_PATH = 'SimpleBaseLib4Pascal';
  QR_CODE_PATH = 'QRCodeGenLib4Pascal';
  COMBINED_PATH = 'CombinedCryptoLib';

  HASH_LIB_SRC_PATH = 'HashLib4Pascal\HashLib\src';
  CRYPTO_LIB_SRC_PATH = 'CryptoLib4Pascal\CryptoLib\src';
  SIMPLE_BASE_LIB_SRC_PATH = 'SimpleBaseLib4Pascal\SimpleBaseLib\src';

procedure TMainFormNew.AddAllPathsToLibrary;
begin
  TCryptoLibProcessor.BackupPaths(GetSelectedVersions, GetSelectedPlatforms);
  TCryptoLibProcessor.InstallPaths(SIMPLE_BASE_LIB_SRC_PATH,
    GetSelectedVersions, GetSelectedPlatforms);
  TCryptoLibProcessor.InstallPaths(HASH_LIB_SRC_PATH, GetSelectedVersions,
    GetSelectedPlatforms);
  TCryptoLibProcessor.InstallPaths(CRYPTO_LIB_SRC_PATH, GetSelectedVersions,
    GetSelectedPlatforms);
end;

procedure TMainFormNew.BrowseCryptoLibRootAccept(Sender: TObject);
begin
  CryptoLibsRoot.Text := BrowseCryptoLibRoot.Folder;
  SettingsChanged := True;
end;

procedure TMainFormNew.BrowseForCombinedFolderAccept(Sender: TObject);
begin
  CombinedLibraryPath.Text := BrowseForCombinedFolder.Folder;
end;

procedure TMainFormNew.CloneOrUpdate(const LocalPath, GitPath: String);
var
  LibPath: string;
  Clone: Boolean;
begin
  LibPath := TPath.Combine(CryptoLibRootPath, LocalPath);
  if not TDirectory.Exists(LibPath) then
  begin
    TDirectory.CreateDirectory(LibPath);
    Clone := True;
  end
  else
  begin
    Clone := not TFile.Exists(TPath.Combine(LibPath, '.git'));
  end;

  if Clone then
    FCommandList.Add(GitCommand + ' clone ' + GitPath + ' ' + LibPath)
  else
    FCommandList.Add(GitCommand + ' -C ' + LibPath + ' pull ' + GitPath);
end;

procedure TMainFormNew.CloneUpdateActionExecute(Sender: TObject);
begin
  if not TDirectory.Exists(CryptoLibRootPath) then
    TDirectory.CreateDirectory(CryptoLibRootPath);

  CloneOrUpdate(SIMPLE_BASE_LIB_PATH, SIMPLE_BASE_LIB_GIT);
  CloneOrUpdate(HASH_LIB_PATH, HASH_LIB_GIT);
  CloneOrUpdate(CRYPTO_LIB_PATH, CRYPTO_LIB_GIT);
  CloneOrUpdate(QR_CODE_PATH, QR_CODE_GIT);
  RunNextCommand;
end;

procedure TMainFormNew.CloseAppButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TMainFormNew.CombinedLibraryPathChange(Sender: TObject);
begin
  SettingsChanged := True;
end;

procedure TMainFormNew.CombinedPathCopyActionExecute(Sender: TObject);
begin
  Clipboard.AsText := CombinedLibraryPath.Text;
end;

function TMainFormNew.CombineLibs: string;
begin
  Result := Trim(CombinedLibraryPath.Text);

  if Result = '' then
    Result := TPath.Combine(GetCryptoLibRootPath, COMBINED_PATH);

  if TDirectory.Exists(Result) then
    TDirectory.Delete(Result);
  TDirectory.CreateDirectory(Result);

  LogThis('Copying standard files');
  TCryptoLibProcessor.CopyStandardFiles(TPath.Combine(CryptoLibRootPath,
    CRYPTO_LIB_PATH), Result);

  TCryptoLibProcessor.AddToCombinedLib(TPath.Combine(CryptoLibRootPath,
    SIMPLE_BASE_LIB_SRC_PATH), Result, LogThis);
  TCryptoLibProcessor.AddToCombinedLib(TPath.Combine(CryptoLibRootPath,
    HASH_LIB_SRC_PATH), Result, LogThis);
  TCryptoLibProcessor.AddToCombinedLib(TPath.Combine(CryptoLibRootPath,
    CRYPTO_LIB_SRC_PATH), Result, LogThis);
end;

procedure TMainFormNew.CryptoLibsRootChange(Sender: TObject);
begin
  SettingsChanged := True;
end;

procedure TMainFormNew.DosCommandTerminated(Sender: TObject);
begin
  RunNextCommand;
end;

procedure TMainFormNew.FindGitAccept(Sender: TObject);
begin
  GitPath.Text := FindGit.Dialog.FileName;
  SettingsChanged := True;
end;

procedure TMainFormNew.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if SettingsChanged then
  begin
    if MessageDlg('Your settings have changed. Do you want to save them?',
      mtConfirmation, [MbYes, mbNo], 0) = mrYes then
      SaveToRegistry;
  end;

  if FCommandList.Count > 0 then
  begin
    if not MessageDlg
      ('There are still Git commands in the queue. Do you want to Close anyway?',
      mtConfirmation, [MbYes, mbNo], 0) = mrYes then
    begin
      CanClose := False;
      Exit;
    end;
  end;

  FCommandList.Free;
end;

procedure TMainFormNew.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  FCommandList := TStringList.Create;
  FDelphiVersions := TDelphiVersions.Create;
  for I := 0 to FDelphiVersions.AvailableCount - 1 do
    InstallForGroup.Items.Add(FDelphiVersions.AvailableVersionName[I]);
  FDelphiVersions.SupportedPlatformsToStrings(InstallForGroup.Items[0],
    Platforms.Items);
  LoadFromRegistry;
end;

function TMainFormNew.GetCryptoLibRootPath: String;
begin
  Result := CryptoLibsRoot.Text;
  Result := Result.Trim;
end;

function TMainFormNew.GetSelectedPlatforms: TStringDynarray;
var
  I, X: Integer;
begin
  if Platforms.SelCount < 1 then
  begin
    MessageDlg('Please select the platforms you need', mtWarning, [mbOK], 0);
    Exit;
  end;
  X := -1;
  SetLength(Result, Platforms.SelCount);
  for I := 0 to Platforms.Count - 1 do
    if Platforms.Checked[I] then
    begin
      Inc(X);
      Result[X] := Platforms.Items[I];
    end;
end;

function TMainFormNew.GetSelectedVersions: TStringDynarray;
var
  I, X: Integer;
begin
  if InstallForGroup.SelCount < 1 then
  begin
    MessageDlg('Please select the versions you need', mtWarning, [mbOK], 0);
    Exit;
  end;
  X := -1;
  SetLength(Result, InstallForGroup.SelCount);
  for I := 0 to InstallForGroup.Count - 1 do
    if InstallForGroup.Checked[I] then
    begin
      Inc(X);
      Result[X] := InstallForGroup.Items[I];
    end;
end;

function TMainFormNew.GitCommand: string;
begin
  Result := GitPath.Text;
  if Result.IsEmpty then
    Result := 'git';
end;

procedure TMainFormNew.GitPathChange(Sender: TObject);
begin
  SettingsChanged := True;
end;

procedure TMainFormNew.InstallForGroupClickCheck(Sender: TObject);
begin
  SettingsChanged := True;
end;

procedure TMainFormNew.InstallMethodChange(Sender: TObject);
begin
  InstallForGroup.Enabled := (InstallMethod.ItemIndex > 0) and
    (InstallMethod.ItemIndex < 4);
  Platforms.Enabled := InstallForGroup.Enabled;
  SettingsChanged := True;
end;

procedure TMainFormNew.InstallToDelphi;
var
  lCombinedPath: string;
begin
  CombinedPathCopyAction.Enabled := False;
  if InstallMethod.ItemIndex = 3 then
    Exit;

  if InstallMethod.ItemIndex < 2 then
  begin
    lCombinedPath := CombineLibs;
    LogThis('Combined library created');
    if InstallMethod.ItemIndex = 0 then
    begin
      Clipboard.AsText := lCombinedPath;
      LogThis('Combined Library Path copied to clipboard');
      CombinedLibraryPath.Text := lCombinedPath;
      CombinedPathCopyAction.Enabled := True;
    end
    else
    begin
      TCryptoLibProcessor.InstallPath(lCombinedPath, GetSelectedVersions,
        GetSelectedPlatforms);
      LogThis('Combined Library Path added to Delphi Library paths');
    end;
  end
  else
  begin
    AddAllPathsToLibrary;
  end;
end;

procedure TMainFormNew.LoadFromRegistry;
var
  Registry: TRegistry;
  ts: TStringList;
  I, X: Integer;
begin
  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.KeyExists(REGISTRY_KEY) then
    begin
      Registry.OpenKey(REGISTRY_KEY, False);
      if Registry.ValueExists('GitPath') then
        GitPath.Text := Registry.ReadString('GitPath');
      if Registry.ValueExists('CryptoLibRoot') then
        CryptoLibsRoot.Text := Registry.ReadString('CryptoLibRoot');
      if Registry.ValueExists('CombinedLibPath') then
        CombinedLibraryPath.Text := Registry.ReadString('CombinedLibPath');

      if Registry.ValueExists('InstallMethod') then
        InstallMethod.ItemIndex := Registry.ReadInteger('InstallMethod');

      if Registry.ValueExists('InstallFor') then
      begin
        ts := TStringList.Create;
        try
          ts.CommaText := Registry.ReadString('InstallFor');
          for I := 0 to ts.Count - 1 do
          begin
            X := InstallForGroup.Items.IndexOf(ts[I]);
            if X > -1 then
              InstallForGroup.Checked[X] := True;
          end;
        finally
          ts.Free;
        end;
      end;

      if Registry.ValueExists('Platforms') then
      begin
        ts := TStringList.Create;
        try
          ts.CommaText := Registry.ReadString('Platforms');
          for I := 0 to ts.Count - 1 do
          begin
            X := Platforms.Items.IndexOf(ts[I]);
            if X > -1 then
              Platforms.Checked[X] := True;
          end;
        finally
          ts.Free;
        end;
      end;

      InstallForGroup.Enabled := (InstallMethod.ItemIndex > 0) and
        (InstallMethod.ItemIndex < 4);
      Platforms.Enabled := InstallForGroup.Enabled;
    end;
  finally
    Registry.Free;
  end;

  SettingsChanged := False;
end;

procedure TMainFormNew.LogThis(const Value: String);
begin
  LogMemo.Lines.Add(Value);
end;

procedure TMainFormNew.PlatformsClickCheck(Sender: TObject);
begin
  SettingsChanged := True;
end;

procedure TMainFormNew.RunNextCommand;
begin
  if DosCommand.IsRunning then
    Exit;

  if FCommandList.Count = 0 then
  begin
    LogThis('All Git commands run');
    InstallToDelphi;
    Exit;
  end;

  LogThis(FCommandList[0]);
  Application.ProcessMessages;

  DosCommand.CommandLine := FCommandList[0];
  FCommandList.Delete(0);
  DosCommand.Execute;
end;

procedure TMainFormNew.SaveSettingsActionExecute(Sender: TObject);
begin
  SaveToRegistry;
end;

procedure TMainFormNew.SaveToRegistry;
var
  Registry: TRegistry;
  S: String;
  I: Integer;
begin
  if not SettingsChanged then
    Exit;
  Registry := TRegistry.Create(KEY_WRITE);
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    Registry.OpenKey(REGISTRY_KEY, True);
    Registry.WriteString('GitPath', GitPath.Text);
    Registry.WriteString('CryptoLibRoot', CryptoLibRootPath);
    Registry.WriteInteger('InstallMethod', InstallMethod.ItemIndex);
    Registry.WriteString('CombinedLibPath', CombinedLibraryPath.Text);

    S := '';
    for I := 0 to InstallForGroup.Count - 1 do
      if InstallForGroup.Checked[I] then
        S := S + InstallForGroup.Items[I] + ',';
    Registry.WriteString('InstallFor', S.Trim([',']));

    S := '';
    for I := 0 to Platforms.Count - 1 do
      if Platforms.Checked[I] then
        S := S + Platforms.Items[I] + ',';
    Registry.WriteString('Platforms', S.Trim([',']));
  finally
    Registry.Free;
  end;
  SettingsChanged := False;
end;

procedure TMainFormNew.SetSettingsChanged(const Value: Boolean);
begin
  FSettingsChanged := Value;
  SaveSettingsAction.Enabled := FSettingsChanged;
  CloneUpdateAction.Enabled := GetCryptoLibRootPath <> '';
end;

end.
