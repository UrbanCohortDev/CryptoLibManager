object MainFormNew: TMainFormNew
  Left = 0
  Top = 0
  Caption = 'Manager for Ugo'#39's Crypto Libraries'
  ClientHeight = 619
  ClientWidth = 687
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 15
  object GitPathLabel: TLabel
    Left = 16
    Top = 8
    Width = 79
    Height = 15
    Caption = 'Git Application'
  end
  object CryptoLibRootLabel: TLabel
    Left = 8
    Top = 56
    Width = 371
    Height = 15
    Caption = 
      'CryptoLib root path (the required libraries will be created in t' +
      'his folder)'
  end
  object InstallMethodLabel: TLabel
    Left = 8
    Top = 110
    Width = 433
    Height = 15
    Caption = 
      'Delphi Install Method for the crypto libraries (your on your own' +
      ' with the QR Code)'
  end
  object CombinedLibraryPathLabel: TLabel
    Left = 8
    Top = 268
    Width = 125
    Height = 15
    Caption = 'Combined Library Path:'
  end
  object InstallForLabel: TLabel
    Left = 8
    Top = 160
    Width = 51
    Height = 15
    Caption = 'Install For'
  end
  object PlatformsLabel: TLabel
    Left = 175
    Top = 160
    Width = 116
    Height = 15
    Caption = 'Platforms (if installed)'
  end
  object LogMemo: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 373
    Width = 681
    Height = 243
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitTop = 195
    ExplicitWidth = 618
  end
  object GitPath: TEdit
    Left = 8
    Top = 29
    Width = 513
    Height = 23
    TabOrder = 1
    OnChange = GitPathChange
  end
  object GitPathButton: TButton
    Left = 527
    Top = 28
    Width = 75
    Height = 25
    Action = FindGit
    TabOrder = 2
  end
  object CryptoLibsRoot: TEdit
    Left = 8
    Top = 77
    Width = 513
    Height = 23
    TabOrder = 3
    OnChange = CryptoLibsRootChange
  end
  object CryptoLibRootButton: TButton
    Left = 528
    Top = 77
    Width = 75
    Height = 25
    Action = BrowseCryptoLibRoot
    TabOrder = 4
  end
  object InstallMethod: TComboBox
    Left = 8
    Top = 131
    Width = 649
    Height = 23
    TabOrder = 5
    Text = 
      'Create a single directory of all units (good for mapping straigh' +
      't into a project)'
    OnChange = InstallMethodChange
    Items.Strings = (
      
        'Create a single directory of all units (good for mapping straigh' +
        't into a project)'
      
        'Create a single directory of all units and map it into Delphi (j' +
        'ust means one line in the library path)'
      
        'Map the invidual directories into delphi (one line for each dire' +
        'ctory- more than one for each lib - in the library path)'
      'Don'#39't install')
  end
  object CombinedLibraryPath: TEdit
    Left = 8
    Top = 289
    Width = 513
    Height = 23
    TabOrder = 6
    OnChange = CombinedLibraryPathChange
  end
  object CombinedPathCopyButton: TButton
    Left = 609
    Top = 288
    Width = 75
    Height = 25
    Action = CombinedPathCopyAction
    TabOrder = 7
  end
  object InstallForGroup: TCheckListBox
    Left = 8
    Top = 181
    Width = 161
    Height = 76
    ItemHeight = 17
    TabOrder = 8
    OnClickCheck = InstallForGroupClickCheck
  end
  object Platforms: TCheckListBox
    Left = 175
    Top = 181
    Width = 482
    Height = 76
    Columns = 4
    ItemHeight = 17
    TabOrder = 9
    OnClickCheck = PlatformsClickCheck
  end
  object Button2: TButton
    Left = 528
    Top = 288
    Width = 75
    Height = 25
    Action = BrowseForCombinedFolder
    TabOrder = 10
  end
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 326
    Width = 681
    Height = 41
    Align = alBottom
    TabOrder = 11
    ExplicitTop = 148
    ExplicitWidth = 618
    object Button1: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 89
      Height = 33
      Action = CloneUpdateAction
      Align = alLeft
      TabOrder = 0
    end
    object SaveSettingsButton: TButton
      AlignWithMargins = True
      Left = 99
      Top = 4
      Width = 83
      Height = 33
      Action = SaveSettingsAction
      Align = alLeft
      TabOrder = 1
    end
    object CloseAppButton: TButton
      AlignWithMargins = True
      Left = 602
      Top = 4
      Width = 75
      Height = 33
      Align = alRight
      Caption = 'Close App'
      TabOrder = 2
      OnClick = CloseAppButtonClick
      ExplicitLeft = 539
    end
  end
  object ActionList1: TActionList
    Left = 392
    Top = 440
    object FindGit: TFileOpen
      Category = 'File'
      Caption = '&Open...'
      Dialog.FileName = 'Git.exe'
      Dialog.Filter = 'Git Executable (*.exe)|*.exe'
      Hint = 'Git|Git Exectuable'
      ImageIndex = 0
      ShortCut = 16463
      OnAccept = FindGitAccept
    end
    object CloneUpdateAction: TAction
      Caption = 'Clone/Update'
      OnExecute = CloneUpdateActionExecute
    end
    object BrowseCryptoLibRoot: TBrowseForFolder
      Category = 'File'
      Caption = 'Browse...'
      DialogCaption = 'Browse For Folder'
      BrowseOptions = []
      BrowseOptionsEx = []
      OnAccept = BrowseCryptoLibRootAccept
    end
    object SaveSettingsAction: TAction
      Caption = 'Save Settings'
      Enabled = False
      OnExecute = SaveSettingsActionExecute
    end
    object CombinedPathCopyAction: TAction
      Caption = 'Copy'
      Enabled = False
      OnExecute = CombinedPathCopyActionExecute
    end
    object BrowseForCombinedFolder: TBrowseForFolder
      Category = 'File'
      Caption = 'Browse...'
      DialogCaption = 'Browse...'
      BrowseOptions = []
      BrowseOptionsEx = []
      OnAccept = BrowseForCombinedFolderAccept
    end
  end
  object DosCommand: TDosCommand
    InputToOutput = False
    MaxTimeAfterBeginning = 0
    MaxTimeAfterLastOutput = 0
    OnTerminated = DosCommandTerminated
    Left = 472
    Top = 440
  end
end
