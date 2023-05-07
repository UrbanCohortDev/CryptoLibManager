unit CryptoLibProcessor;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.Types,
  System.Classes,
  UC.Delphi.Versions;

type
  TLogProcess = procedure(const LogMessage: string) of object;

  TCryptoLibProcessor = class
  private
    class procedure ProcessThisFolder(const AFolder, ACombinedPath: string;
      const ALevel: Integer; ALogProcess: TLogProcess);
    class procedure AddPathsToStrings(const APath: string; AList: TStrings);
    class procedure InstallPathsToRegistry(const AVersion, APlatform: String;
      PathList: TStrings);
  public
    class procedure AddToCombinedLib(const SourcePath, CombinedPath: string;
      ALogProcess: TLogProcess);
    class procedure CopyStandardFiles(const SourcePath, CombinedPath: string);
    class procedure InstallPath(const APath: string;
      const AVersions, APlatforms: TStringDynArray);
    class procedure InstallPaths(const SourcePath: string;
      const AVersions, APlatforms: TStringDynArray);
    class procedure BackupPaths(const AVersions, APlatforms: TStringDynArray);
  end;

implementation

uses
  System.StrUtils,
  System.Win.Registry,
  WinAPI.Windows,
  System.IOUtils;

const
  DELPHI_SEARCH_PATH = 'Search Path';
  BACKUP_SEARCH_PATH = 'Search Path Backup UC';

  { TCryptoLibProcessor }

class procedure TCryptoLibProcessor.AddPathsToStrings(const APath: string;
  AList: TStrings);
var
  lFolders: TStringDynArray;
  I: Integer;
begin
  lFolders := TDirectory.GetDirectories(APath);
  for I := 0 to Length(lFolders) - 1 do
  begin
    AList.Add(lFolders[I]);
    AddPathsToStrings(lFolders[I], AList);
  end;
end;

class procedure TCryptoLibProcessor.AddToCombinedLib(const SourcePath,
  CombinedPath: string; ALogProcess: TLogProcess);
var
  lFolders: TStringDynArray;
  I: Integer;
begin
  lFolders := TDirectory.GetDirectories(SourcePath);
  for I := 0 to Length(lFolders) - 1 do
    ProcessThisFolder(lFolders[I], CombinedPath, 1, ALogProcess);
end;

class procedure TCryptoLibProcessor.BackupPaths(const AVersions,
  APlatforms: TStringDynArray);
var
VI, PI: Integer;
  Registry: TRegistry;
  S: string;
begin
  for VI := 0 to Length(AVersions) - 1 do
  begin
    for PI := 0 to Length(APlatforms) do
    begin
      Registry := TRegistry.Create(KEY_READ);
      try
        Registry.RootKey := HKEY_CURRENT_USER;
        if Registry.OpenKey(DelphiVersions.LibraryKey(AVersions[VI]) + '\' +
          APlatforms[PI], False) and Registry.KeyExists(DELPHI_SEARCH_PATH) then
        begin
          S := Registry.ReadString(DELPHI_SEARCH_PATH);
          Registry.Access := KEY_WRITE;
          Registry.WriteString(BACKUP_SEARCH_PATH, S);
        end;
      finally
        Registry.Free;
      end;
    end;
  end;
end;

class procedure TCryptoLibProcessor.CopyStandardFiles(const SourcePath,
  CombinedPath: string);
var
  lFiles: TStringDynArray;
  lPath: String;
  I: Integer;
begin
  lFiles := TDirectory.GetFiles(SourcePath);
  for I := 0 to Length(lFiles) - 1 do
  begin
    if SameText(TPath.GetFileName(lFiles[I]), 'README.md') then
      TFile.Copy(lFiles[I], TPath.Combine(CombinedPath, 'README.md'))
    else if SameText(TPath.GetFileName(lFiles[I]), 'LICENSE') then
    begin
      lPath := TPath.Combine(CombinedPath, 'LICENSE') + '.txt';
      TFile.Copy(lFiles[I], lPath);
    end;
  end;
end;

class procedure TCryptoLibProcessor.InstallPath(const APath: string;
  const AVersions, APlatforms: TStringDynArray);
var
  VI, PI: Integer;
  Registry: TRegistry;
  S: string;
begin
  for VI := 0 to Length(AVersions) - 1 do
  begin
    for PI := 0 to Length(APlatforms) do
    begin
      Registry := TRegistry.Create(KEY_READ);
      try
        Registry.RootKey := HKEY_CURRENT_USER;
        if Registry.OpenKey(DelphiVersions.LibraryKey(AVersions[VI]) + '\' +
          APlatforms[PI], False) and Registry.KeyExists(DELPHI_SEARCH_PATH) then
        begin
          S := Registry.ReadString(DELPHI_SEARCH_PATH);
          if S.Contains(APath) then
             Exit;
          S := S + IfThen(S.EndsWith(';'), '', ';') + APath;
          Registry.Access := KEY_WRITE;
          Registry.WriteString(DELPHI_SEARCH_PATH, S);
        end;
      finally
        Registry.Free;
      end;
    end;
  end;
end;

class procedure TCryptoLibProcessor.InstallPaths(const SourcePath: string;
  const AVersions, APlatforms: TStringDynArray);
var
  TS: TStringList;
  VI, PI: Integer;
begin
  TS := TStringList.Create;
  try
    TS.Delimiter := ';';
    TS.StrictDelimiter := True;
    AddPathsToStrings(SourcePath, TS);

    for VI := 0 to Length(AVersions) - 1 do
      for PI := 0 to Length(APlatforms) do
        InstallPathsToRegistry(AVersions[VI], APlatforms[PI], TS);

  finally
    TS.Free;
  end;
end;

class procedure TCryptoLibProcessor.InstallPathsToRegistry(const AVersion,
  APlatform: String; PathList: TStrings);
var
  Registry: TRegistry;
  RegistryList: TStringList;
  originalSearchPath, newSearchPath: string;
  I, A: Integer;
begin
  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKey(DelphiVersions.LibraryKey(AVersion) + '\' + APlatform,
      False) and Registry.KeyExists(DELPHI_SEARCH_PATH) then
    begin
      RegistryList := TStringList.Create;
      try
        RegistryList.Delimiter := ';';
        RegistryList.StrictDelimiter := True;

        originalSearchPath := Registry.ReadString(DELPHI_SEARCH_PATH);
        RegistryList.DelimitedText := originalSearchPath;
        A := 0;
        for I := 0 to PathList.Count - 1 do
        begin
          if RegistryList.IndexOf(PathList[I]) = -1 then
          begin
            RegistryList.Add(PathList[I]);
            Inc(A);
          end;
        end;

        if A > 0 then
        begin
          newSearchPath := RegistryList.DelimitedText;
          Registry.Access := KEY_WRITE;
          Registry.WriteString(DELPHI_SEARCH_PATH, newSearchPath);
        end;
      finally
        RegistryList.Free;
      end;
    end;
  finally
    Registry.Free;
  end;
end;

class procedure TCryptoLibProcessor.ProcessThisFolder(const AFolder,
  ACombinedPath: string; const ALevel: Integer; ALogProcess: TLogProcess);
var
  lFiles, lFolders: TStringDynArray;
  S, lName, lTargetName, fileContents, lInc: string;
begin
  if Assigned(ALogProcess) then
    ALogProcess(' ' + StringOfChar('-', ALevel * 2) + ' Processing ' + AFolder);

  lFolders := TDirectory.GetDirectories(AFolder);
  lFiles := TDirectory.GetFiles(AFolder);
  for S in lFiles do
  begin
    lName := TPath.GetFileName(S);
    lTargetName := TPath.Combine(ACombinedPath, lName);
    if TPath.GetExtension(S).ToLower = '.pas' then
    begin
      fileContents := TFile.ReadAllText(S);
      lInc := '{$I ' + DupeString('..\', ALevel) + 'Include\';
      fileContents := fileContents.Replace(lInc, '{$I ', [rfReplaceAll]);
      TFile.WriteAllText(lTargetName, fileContents);
    end
    else
    begin
      TFile.Copy(S, lTargetName);
    end;
  end;

  for S in lFolders do
    ProcessThisFolder(S, ACombinedPath, ALevel + 1, ALogProcess);
end;

end.
