unit main;

//TODO: at45 установка размера странцы
//TODO: at45 Проверка размера страницы перед операциями


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Menus, ActnList, Buttons, StrUtils, spi25,spi25nand,
  spi45, spi95, i2c, microwire, spimulti, ft232hhw,
  XMLRead, XMLWrite, DOM, msgstr, Translations, LCLProc, LCLType, LCLTranslator,
  LResources, MPHexEditorEx, MPHexEditor, search, sregedit,
  utilfunc, findchip, DateUtils, lazUTF8,
  pascalc, ScriptsFunc, ScriptEdit, baseHW, UsbAspHW, ch341hw, ch347hw, avrisphw, arduinohw;

type

  { TMainForm }

  TMainForm = class(TForm)
    CheckBox_I2C_A1: TToggleBox;
    CheckBox_I2C_A0: TToggleBox;
    CheckBox_I2C_ByteRead: TCheckBox;
    CheckBox_I2C_DevA6: TToggleBox;
    CheckBox_I2C_DevA5: TToggleBox;
    CheckBox_I2C_DevA4: TToggleBox;
    CheckBox_I2C_A2: TToggleBox;
    ComboAddrType: TComboBox;
    ComboBox_chip_scriptrun: TComboBox;
    ComboPageSpare: TComboBox;
    ComboPageSpareSize: TComboBox;
    ComboSPICMD: TComboBox;
    ComboChipSize: TComboBox;
    ComboMWBitLen: TComboBox;
    ComboPageSize: TComboBox;
    Label6: TLabel;
    LabelSpare: TLabel;
    Label_StartAddress: TLabel;
    Label_StartPage: TLabel;
    MenuHWFT232H: TMenuItem;
    MenuFT232SPIClock: TMenuItem;
    MenuFT232SPI30Mhz: TMenuItem;
    MenuFT232SPI6Mhz: TMenuItem;
    MenuHWCH347: TMenuItem;
    MenuCH347SPIClock: TMenuItem;
    MenuCH347SPIClock468_75KHz: TMenuItem;
    MenuCH347SPIClock60MHz: TMenuItem;
    MenuCH347SPIClock30MHz: TMenuItem;
    MenuCH347SPIClock15MHz: TMenuItem;
    MenuCH347SPIClock7_5MHz: TMenuItem;
    MenuCH347SPIClock3_75MHz: TMenuItem;
    MenuCH347SPIClock1_875MHz: TMenuItem;
    MenuCH347SPIClock937_5KHz: TMenuItem;
    MenuSendAB: TMenuItem;
    StartAddressEdit: TEdit;
    GroupChipSettings: TGroupBox;
    ImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label_chip_scripts: TLabel;
    Label_I2C_DevAddr: TLabel;
    LabelSPICMD: TLabel;
    LabelChipName: TLabel;
    MainMenu: TMainMenu;
    Log: TMemo;
    Menu32Khz: TMenuItem;
    Menu93_75Khz: TMenuItem;
    MenuChip: TMenuItem;
    MenuAutoCheck: TMenuItem;
    ComboItem1: TMenuItem;
    Menu3Mhz: TMenuItem;
    MenuIgnoreBusyBit: TMenuItem;
    MenuGotoOffset: TMenuItem;
    MenuFind: TMenuItem;
    MenuItem1: TMenuItem;
    MenuCopyToClip: TMenuItem;
    CopyLogMenuItem: TMenuItem;
    ClearLogMenuItem: TMenuItem;
    MenuHWUSBASP: TMenuItem;
    MenuHWCH341A: TMenuItem;
    MenuFindChip: TMenuItem;
    MenuHWAVRISP: TMenuItem;
    MenuAVRISPSPIClock: TMenuItem;
    MenuAVRISP8MHz: TMenuItem;
    MenuAVRISP4MHz: TMenuItem;
    MenuAVRISP2MHz: TMenuItem;
    MenuAVRISP1MHz: TMenuItem;
    MenuAVRISP500KHz: TMenuItem;
    MenuAVRISP250KHz: TMenuItem;
    MenuAVRISP125KHz: TMenuItem;
    LangMenuItem: TMenuItem;
    BlankCheckMenuItem: TMenuItem;
    AllowInsertItem: TMenuItem;
    MenuHWARDUINO: TMenuItem;
    MenuArduinoSPIClock: TMenuItem;
    MenuArduinoISP8MHz: TMenuItem;
    MenuArduinoISP4MHz: TMenuItem;
    MenuArduinoISP2MHz: TMenuItem;
    MenuArduinoISP1MHz: TMenuItem;
    MenuArduinoCOMPort: TMenuItem;
    MenuSkipFF: TMenuItem;
    MPHexEditorEx: TMPHexEditorEx;
    ScriptsMenuItem: TMenuItem;
    MenuItemHardware: TMenuItem;
    MenuItemBenchmark: TMenuItem;
    MenuItemEditSreg: TMenuItem;
    MenuItemReadSreg: TMenuItem;
    MenuItemLockFlash: TMenuItem;
    MenuItem4: TMenuItem;
    MenuMW8Khz: TMenuItem;
    MenuMW16Khz: TMenuItem;
    MenuMicrowire: TMenuItem;
    MenuMW32Khz: TMenuItem;
    MenuMWClock: TMenuItem;
    MenuOptions: TMenuItem;
    MenuSPI: TMenuItem;
    MenuSPIClock: TMenuItem;
    Menu1_5Mhz: TMenuItem;
    Menu750Khz: TMenuItem;
    Menu375Khz: TMenuItem;
    Menu187_5Khz: TMenuItem;
    OpenDialog: TOpenDialog;
    DropDownMenu: TPopupMenu;
    EditorPopupMenu: TPopupMenu;
    LogPopupMenu: TPopupMenu;
    DropdownMenuLock: TPopupMenu;
    Panel_I2C_DevAddr: TPanel;
    BlankCheckDropDownMenu: TPopupMenu;
    ProgressBar: TProgressBar;
    RadioI2C: TRadioButton;
    RadioMw: TRadioButton;
    RadioSPI: TRadioButton;
    SaveDialog: TSaveDialog;
    SpeedButton1: TSpeedButton;
    Splitter1: TSplitter;
    StartPageEdit: TEdit;
    StatusBar: TStatusBar;
    CheckBox_I2C_DevA7: TToggleBox;
    ToolBar: TToolBar;
    ButtonRead: TToolButton;
    ButtonWrite: TToolButton;
    ButtonVerify: TToolButton;
    ToolButton1: TToolButton;
    ButtonReadID: TToolButton;
    ButtonErase: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ButtonBlock: TToolButton;
    ButtonOpenHex: TToolButton;
    ButtonSaveHex: TToolButton;
    ButtonCancel: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure BlankCheckMenuItemClick(Sender: TObject);
    procedure ButtonEraseClick(Sender: TObject);
    procedure ButtonReadClick(Sender: TObject);
    procedure ClearLogMenuItemClick(Sender: TObject);
    procedure ComboSPICMDChange(Sender: TObject);
    procedure CopyLogMenuItemClick(Sender: TObject);
    procedure AllowInsertItemClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ChipClick(Sender: TObject);
    procedure ChangeLang(Sender: TObject);
    procedure ComboItem1Click(Sender: TObject);
    procedure MenuArduinoCOMPortClick(Sender: TObject);
    procedure MenuHWARDUINOClick(Sender: TObject);
    procedure MenuHWAVRISPClick(Sender: TObject);
    procedure MenuCopyToClipClick(Sender: TObject);
    procedure MenuFindChipClick(Sender: TObject);
    procedure MenuFindClick(Sender: TObject);
    procedure MenuGotoOffsetClick(Sender: TObject);
    procedure MenuHWCH341AClick(Sender: TObject);
    procedure MenuHWCH347Click(Sender: TObject);
    procedure MenuHWFT232HClick(Sender: TObject);
    procedure MenuHWUSBASPClick(Sender: TObject);
    procedure MenuItemBenchmarkClick(Sender: TObject);
    procedure MenuItemEditSregClick(Sender: TObject);
    procedure MenuItemLockFlashClick(Sender: TObject);
    procedure MenuItemReadSregClick(Sender: TObject);
    procedure MPHexEditorExChange(Sender: TObject);
    procedure RadioI2CChange(Sender: TObject);
    procedure RadioMwChange(Sender: TObject);
    procedure RadioSPIChange(Sender: TObject);
    procedure ButtonWriteClick(Sender: TObject);
    procedure ButtonVerifyClick(Sender: TObject);
    procedure ButtonBlockClick(Sender: TObject);
    procedure ButtonReadIDClick(Sender: TObject);
    procedure ButtonOpenHexClick(Sender: TObject);
    procedure ButtonSaveHexClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure I2C_DevAddrChange(Sender: TObject);
    procedure ScriptsMenuItemClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure StartAddressEditChange(Sender: TObject);
    procedure StartAddressEditKeyPress(Sender: TObject; var Key: char);
    procedure StartPageEditChange(Sender: TObject);
    procedure StartPageEditKeyPress(Sender: TObject; var Key: char);
    procedure VerifyFlash(BlankCheck: boolean = false);
  private
    { private declarations }
  public
    { public declarations }

  end;

  procedure LogPrint(text: string);
  procedure SaveOptions(XMLfile: TXMLDocument);
  Procedure LoadOptions(XMLfile: TXMLDocument);
  procedure LoadXML;
  procedure Translate(XMLfile: TXMLDocument);
  function OpenDevice: boolean;
  function SetSPISpeed(OverrideSpeed: byte): integer;
  procedure SyncUI_ICParam();
  function UserCancel(): boolean;

const
  SPI_CMD_25             = 0;
  SPI_CMD_45             = 1;
  SPI_CMD_KB             = 2;
  SPI_CMD_95             = 3;
  SPI_CMD_25_NAND        = 4; // Thêm dòng này

  ChipListFileName       = 'chiplist.xml';
  SettingsFileName       = 'settings.xml';
  ScriptsPath            = 'scripts'+DirectorySeparator;

type

  TCurrentICParam = record
    Name: string;
    Page: integer;
    Spare: integer; // Thêm trường này SPI NAND
    Size: Longword;
    PagesPerBlock: integer; // Thêm trường này SPI NAND
    Planes: integer; // Thêm trường này planes: 1 hoặc 2 SPI NAND
    SpiCmd: byte;
    I2CAddrType: byte;
    MWAddLen: byte;

    Script: string;
  end;


var
  MainForm: TMainForm;
  ChipListFile: TXMLDocument;
  SettingsFile: TXMLDocument;
  CurrentICParam: TCurrentICParam;
  ScriptEngine: TPasCalc;
  RomF: TMemoryStream;

  AsProgrammer: TAsProgrammer;

  Arduino_COMPort: string;
  Arduino_BaudRate: integer = 921600;

implementation


var
  TimeCounter: TDateTime;
  CurrentLang: string = 'en';

{$R *.lfm}

procedure SyncUI_ICParam();
begin
  CurrentICParam.SpiCmd := MainForm.ComboSPICMD.ItemIndex;
  CurrentICParam.I2CAddrType := MainForm.ComboAddrType.ItemIndex;

  if IsNumber(MainForm.ComboMWBitLen.Text) then
    CurrentICParam.MWAddLen := StrToInt(MainForm.ComboMWBitLen.Text) else
      CurrentICParam.MWAddLen := 0;

  if IsNumber(MainForm.ComboPageSize.Text) then
    CurrentICParam.Page := StrToInt(MainForm.ComboPageSize.Text)
  else if UpCase(MainForm.ComboPageSize.Text) = 'SSTB' then
    CurrentICParam.Page := -1
  else if UpCase(MainForm.ComboPageSize.Text) = 'SSTW' then
    CurrentICParam.Page := -2
  else
    CurrentICParam.Page := 0;

  if IsNumber(MainForm.ComboChipSize.Text) then
    CurrentICParam.Size := StrToInt(MainForm.ComboChipSize.Text) else
      CurrentICParam.Size := 0;
      
  if IsNumber(MainForm.ComboPageSpareSize.Text) then
    CurrentICParam.Spare := StrToInt(MainForm.ComboPageSpareSize.Text) else
      CurrentICParam.Spare := 0;

  // Thêm dòng này để lấy PagesPerBlock
  // Giả sử bạn có một hàm để lấy PagesPerBlock từ chiplist.xml hoặc từ UI
  // CurrentICParam.PagesPerBlock := GetPagesPerBlockFromChipList(CurrentICParam.Name);
  // Hoặc nếu bạn muốn người dùng nhập thủ công, thêm một ComboBoxPagesPerBlock
  // CurrentICParam.PagesPerBlock := StrToInt(MainForm.ComboPagesPerBlock.Text);
  // Nếu không có, đặt mặc định là 64
  if CurrentICParam.PagesPerBlock = 0 then
    CurrentICParam.PagesPerBlock := 64;

  // planes Nếu không có, đặt mặc định là 1
  if CurrentICParam.Planes = 0 then
    CurrentICParam.Planes := 1;

end;

function UserCancel(): boolean;
begin
  Result := false;
  if MainForm.ButtonCancel.Tag <> 0 then
  begin
    LogPrint(STR_USER_CANCEL);
    MainForm.ProgressBar.Style := pbstNormal;
     MainForm.ProgressBar.Position:= 0;
    Result := true;
  end;
end;

procedure LoadXML;
var
  RootNode: TDOMNode;
begin
  ChipListFile := nil;
  SettingsFile := nil;
  if FileExists(ChipListFileName) then
  begin
    try
      ReadXMLFile(ChipListFile, ChipListFileName);
    except
      on E: EXMLReadError do
      begin
        ShowMessage(E.Message);
        ChipListFile := nil;
      end;
    end;
  end;

  if FileExists(SettingsFileName) then
  begin
    try
      ReadXMLFile(SettingsFile, SettingsFileName);
    except
      on E: EXMLReadError do
      begin
        ShowMessage(E.Message);
        SettingsFile := nil;
      end;
    end;
  end else
  begin
    SettingsFile := TXMLDocument.Create;
    // Create a root node
    RootNode := SettingsFile.CreateElement('settings');
    SettingsFile.Appendchild(RootNode);
  end;

end;

procedure TMainForm.ChangeLang(Sender: TObject);
begin
  CurrentLang := TMenuItem(Sender).Hint;

  Translations.TranslateResourceStrings(GetCurrentDir + '/lang/' + CurrentLang + '.po');
  LRSTranslator.Free;
  LRSTranslator:= TPOTranslator.Create(GetCurrentDir + '/lang/' + CurrentLang + '.po');
  TPOTranslator(LRSTranslator).UpdateTranslation(MainForm);
  TPOTranslator(LRSTranslator).UpdateTranslation(ScriptEditForm);
  TPOTranslator(LRSTranslator).UpdateTranslation(ChipSearchForm);
  TPOTranslator(LRSTranslator).UpdateTranslation(sregeditForm);
  TPOTranslator(LRSTranslator).UpdateTranslation(SearchForm);
end;

procedure LoadLangList();
var
  LangDir: string;
  LangName: string;
  LangFile: Text;
  SearchRec : TSearchRec;
  MenuItem: TMenuItem;
begin
  LangDir := GetCurrentDir + '/lang/';

  If FindFirstUTF8(LangDir+'*.po', faAnyFile, SearchRec) = 0 then
  begin
    Repeat
      AssignFile(LangFile, LangDir+SearchRec.Name);
      Reset(LangFile);
      ReadLn(LangFile, LangName);
      CloseFile(LangFile);
      Delete(LangName, 1, 1);

      MenuItem := NewItem(LangName, 0, False, True, @MainForm.ChangeLang, 0, '');
      MenuItem.Hint := ExtractFileNameOnly(SearchRec.Name);
      MenuItem.AutoCheck := true;
      MenuItem.RadioItem := true;
      MainForm.LangMenuItem.Add(MenuItem);
      if MenuItem.Hint = Currentlang then MenuItem.Checked := true;

    Until FindNextUTF8(SearchRec) <> 0;
  end;

  FindCloseUTF8(SearchRec);
end;

procedure Translate(XMLfile: TXMLDocument);
var
   PODirectory: String;
   Node: TDOMNode;
begin

  PODirectory:= GetCurrentDir + '/lang/';
  CurrentLang:='';

  if XMLfile <> nil then
  begin

      Node := XMLfile.DocumentElement.FindNode('locale');

      if (Node <> nil) then
      if (Node.HasAttributes) then
      begin

        if  Node.Attributes.GetNamedItem('lang') <> nil then
          CurrentLang := UTF16ToUTF8(Node.Attributes.GetNamedItem('lang').NodeValue);

      end;
  end;

  if CurrentLang = '' then
  begin
    CurrentLang := 'en';
    Exit;
  end;

  if FileExistsUTF8(PODirectory + CurrentLang + '.po') then
  begin
    LRSTranslator:= TPOTranslator.Create(PODirectory + CurrentLang + '.po');
    Translations.TranslateResourceStrings(PODirectory + CurrentLang + '.po');
  end;

end;               

procedure LogPrint(text: string);
begin
  MainForm.Log.Lines.Add(text);
end;


function OpenDevice: boolean;
begin

  if not AsProgrammer.Programmer.DevOpen then
  begin
    LogPrint(AsProgrammer.Programmer.GetLastError);
    result := false;
    Exit;
  end;

  LogPrint(STR_CURR_HW+AsProgrammer.Programmer.HardwareName);
  result := true
end;


function IsLockBitsEnabled: boolean;
var
  sreg: byte;
begin
  result := false;
  sreg := 0;
  if MainForm.ComboSPICMD.ItemIndex = SPI_CMD_25 then
  begin
    UsbAsp25_ReadSR(sreg);
    if IsBitSet(sreg, 2) or
       IsBitSet(sreg, 3) or
       IsBitSet(sreg, 4) or
       IsBitSet(sreg, 5) or
       IsBitSet(sreg, 6) or
       IsBitSet(sreg, 7)
    then
    begin
      LogPrint(STR_BLOCK_EN);
      Result := true;
    end;
  end;

  if MainForm.ComboSPICMD.ItemIndex = SPI_CMD_45 then
  begin
    UsbAsp45_ReadSR(sreg);
    if (sreg and 2 <> 0) then
    begin
      LogPrint(STR_BLOCK_EN);
      Result := true;
    end;
  end;

end;

//Установка скорости spi и Microwire
function SetSPISpeed(OverrideSpeed: byte): integer;
var
  Speed: byte;
begin
  if AsProgrammer.Current_HW = CHW_ARDUINO then
  begin
    if MainForm.MenuArduinoISP8Mhz.Checked then Speed := MainForm.MenuArduinoISP8Mhz.Tag;
    if MainForm.MenuArduinoISP4Mhz.Checked then Speed := MainForm.MenuArduinoISP4Mhz.Tag;
    if MainForm.MenuArduinoISP2Mhz.Checked then Speed := MainForm.MenuArduinoISP2Mhz.Tag;
    if MainForm.MenuArduinoISP1Mhz.Checked then Speed := MainForm.MenuArduinoISP1Mhz.Tag;
  end;

  if AsProgrammer.Current_HW = CHW_AVRISP then
  begin
    if MainForm.MenuAVRISP8Mhz.Checked then Speed := MainForm.MenuAVRISP8Mhz.Tag;
    if MainForm.MenuAVRISP4Mhz.Checked then Speed := MainForm.MenuAVRISP4Mhz.Tag;
    if MainForm.MenuAVRISP2Mhz.Checked then Speed := MainForm.MenuAVRISP2Mhz.Tag;
    if MainForm.MenuAVRISP1Mhz.Checked then Speed := MainForm.MenuAVRISP1Mhz.Tag;
    if MainForm.MenuAVRISP500Khz.Checked then Speed := MainForm.MenuAVRISP500Khz.Tag;
    if MainForm.MenuAVRISP250Khz.Checked then Speed := MainForm.MenuAVRISP250Khz.Tag;
    if MainForm.MenuAVRISP125Khz.Checked then Speed := MainForm.MenuAVRISP125Khz.Tag;
  end;

  if (MainForm.RadioSPI.Checked) and (AsProgrammer.Current_HW = CHW_USBASP) then
  begin
    if MainForm.Menu3Mhz.Checked then Speed := MainForm.Menu3Mhz.Tag;
    if MainForm.Menu1_5Mhz.Checked then Speed := MainForm.Menu1_5Mhz.Tag;
    if MainForm.Menu750Khz.Checked then Speed := MainForm.Menu750Khz.Tag;
    if MainForm.Menu375Khz.Checked then Speed := MainForm.Menu375Khz.Tag;
    if MainForm.Menu187_5Khz.Checked then Speed := MainForm.Menu187_5Khz.Tag;
    if MainForm.Menu93_75Khz.Checked then Speed := MainForm.Menu93_75Khz.Tag;
    if MainForm.Menu32Khz.Checked then Speed := MainForm.Menu32Khz.Tag;
  end;

  if (MainForm.RadioMw.Checked) and (AsProgrammer.Current_HW = CHW_USBASP) then
  begin
    if MainForm.MenuMW32Khz.Checked then Speed := MainForm.MenuMW32Khz.Tag;
    if MainForm.MenuMW16Khz.Checked then Speed := MainForm.MenuMW16Khz.Tag;
    if MainForm.MenuMW8Khz.Checked then Speed := MainForm.MenuMW8Khz.Tag;
  end;

  if (MainForm.RadioSPI.Checked) and (AsProgrammer.Current_HW = CHW_FT232H) then
  begin
    if MainForm.MenuFT232SPI30Mhz.Checked then Speed := MainForm.MenuFT232SPI30Mhz.Tag;
    if MainForm.MenuFT232SPI6Mhz.Checked then Speed := MainForm.MenuFT232SPI6Mhz.Tag;
  end;

  if (MainForm.RadioSPI.Checked) and (AsProgrammer.Current_HW = CHW_CH347) then
  begin
    if MainForm.MenuCH347SPIClock60MHz.Checked then Speed := MainForm.MenuCH347SPIClock60MHz.Tag;
    if MainForm.MenuCH347SPIClock30MHz.Checked then Speed := MainForm.MenuCH347SPIClock30MHz.Tag;
    if MainForm.MenuCH347SPIClock15MHz.Checked then Speed := MainForm.MenuCH347SPIClock15MHz.Tag;
    if MainForm.MenuCH347SPIClock7_5MHz.Checked then Speed := MainForm.MenuCH347SPIClock7_5MHz.Tag;
    if MainForm.MenuCH347SPIClock3_75MHz.Checked then Speed := MainForm.MenuCH347SPIClock3_75MHz.Tag;
    if MainForm.MenuCH347SPIClock1_875MHz.Checked then Speed := MainForm.MenuCH347SPIClock1_875MHz.Tag;
    if MainForm.MenuCH347SPIClock937_5KHz.Checked then Speed := MainForm.MenuCH347SPIClock937_5KHz.Tag;
    if MainForm.MenuCH347SPIClock468_75KHz.Checked then Speed := MainForm.MenuCH347SPIClock468_75KHz.Tag;
  end;

  if OverrideSpeed <> 0 then Speed := OverrideSpeed;

  result := speed;
end;


function SetI2CDevAddr(): byte;
begin
    result := 0;
    With MainForm do
    begin
      if (CheckBox_I2C_A0.Checked) then result := SetBit(result, 1);
      if (CheckBox_I2C_A1.Checked) then result := SetBit(result, 2);
      if (CheckBox_I2C_A2.Checked) then result := SetBit(result, 3);

      if (CheckBox_I2C_DevA4.Checked) then result := SetBit(result, 4);
      if (CheckBox_I2C_DevA5.Checked) then result := SetBit(result, 5);
      if (CheckBox_I2C_DevA6.Checked) then result := SetBit(result, 6);
      if (CheckBox_I2C_DevA7.Checked) then result := SetBit(result, 7);
    end;
end;

procedure ReadFlashMW(var RomStream: TMemoryStream; AddrBitLen: byte; StartAddress, ChipSize: cardinal);
var
  ChunkSize: Word;
  BytesRead: integer;
  DataChunk: array[0..2047] of byte;
  Address: cardinal;
begin
  if (StartAddress >= ChipSize) or (ChipSize = 0) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  ChunkSize := 2;
  if ChunkSize > ChipSize then ChunkSize := ChipSize;

  LogPrint(STR_READING_FLASH);
  BytesRead := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := ChipSize div ChunkSize;

  RomStream.Clear;

  while Address < ChipSize div 2 do
  begin
    //if ChunkSize > ((ChipSize div 2) - Address) then ChunkSize := (ChipSize div 2) - Address;

    BytesRead := BytesRead + UsbAspMW_Read(AddrBitLen, Address, datachunk, ChunkSize);
    RomStream.WriteBuffer(datachunk, ChunkSize);
    Inc(Address, ChunkSize div 2);

    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 2;
    Application.ProcessMessages;

    if UserCancel then Break;
  end;

  if BytesRead <> ChipSize then
    LogPrint(STR_WRONG_BYTES_READ)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;

procedure WriteFlashMW(var RomStream: TMemoryStream; AddrBitLen: byte; StartAddress, ChipSize: cardinal);
var
  DataChunk: array[0..2047] of byte;
  Address, BytesWrite: cardinal;
  ChunkSize: Word;
begin
  if (StartAddress >= ChipSize) or (ChipSize = 0) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  LogPrint(STR_WRITING_FLASH);
  BytesWrite := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := ChipSize;

  ChunkSize := 2;

  if ChunkSize > ChipSize then ChunkSize := ChipSize;

  UsbAspMW_EWEN(AddrBitLen);

  while Address < ChipSize div 2 do
  begin
    RomStream.ReadBuffer(DataChunk, ChunkSize);

    BytesWrite := BytesWrite + UsbAspMW_Write(AddrBitLen, Address, datachunk, ChunkSize);
    Inc(Address, ChunkSize div 2);

    while UsbAspMW_Busy do
    begin
       Application.ProcessMessages;
       if UserCancel then Exit;
    end; 

    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + ChunkSize;
    Application.ProcessMessages;

    if UserCancel then Break;
  end;

  if BytesWrite <> ChipSize then
    LogPrint(STR_WRONG_BYTES_WRITE)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;

procedure WriteFlash25(var RomStream: TMemoryStream; StartAddress, WriteSize: cardinal; PageSize: word; WriteType: integer);
const
  FLASH_SIZE_128MBIT = 16777216;
var
  DataChunk: array[0..2047] of byte;
  DataChunk2: array[0..2047] of byte;
  Address, BytesWrite: cardinal;
  PageSizeTemp: word;
  i: integer;
  SkipPage: boolean = false;
begin
  if (WriteSize = 0) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  if MainForm.MenuAutoCheck.Checked then
    LogPrint(STR_WRITING_FLASH_WCHK) else
      LogPrint(STR_WRITING_FLASH);

  PageSizeTemp := PageSize;
  BytesWrite := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := WriteSize div PageSize;

  if WriteSize > FLASH_SIZE_128MBIT then UsbAsp25_EN4B();

  while (Address-StartAddress) < WriteSize do
  begin
    //Только вначале aai
    if (((WriteType = WT_SSTB) or (WriteType = WT_SSTW)) and (Address = StartAddress)) or
    //Вначале страницы
    (WriteType = WT_PAGE) then UsbAsp25_WREN();

    //Determines first page buffer size to prevent buffer "rolls over" on address boundary
        if (StartAddress > 0) and (Address = StartAddress) and (PageSize > 2) then
           PageSize := (StrToInt(MainForm.ComboChipSize.Text) - StartAddress) mod PageSize else
              PageSize := PageSizeTemp;

    if (WriteSize - (Address-StartAddress)) < PageSize then PageSize := (WriteSize - (Address-StartAddress));
    RomStream.ReadBuffer(DataChunk, PageSize);

    if (WriteType = WT_SSTB) then
      if (Address = StartAddress) then //Пишем первый байт с адресом
        BytesWrite := BytesWrite + UsbAsp25_Write($AF, Address, datachunk, PageSize)
        else
        //Пишем остальные(без адреса)
        BytesWrite := BytesWrite + UsbAsp25_WriteSSTB($AF, datachunk[0]);

    if (WriteType = WT_SSTW) then
      if (Address = StartAddress) then //Пишем первые два байта с адресом
        BytesWrite := BytesWrite + UsbAsp25_Write($AD, Address, datachunk, PageSize)
        else
        //Пишем остальные(без адреса)
        BytesWrite := BytesWrite + UsbAsp25_WriteSSTW($AD, datachunk[0], datachunk[1]);

    if WriteType = WT_PAGE then
    begin
      //Если страница вся FF то не пишем ее
      if MainForm.MenuSkipFF.Checked then
      begin
        SkipPage := True;
        for i:=0 to PageSize-1 do
          if DataChunk[i] <> $FF then
          begin
            SkipPage := False;
            Break;
          end;
      end;

      if not SkipPage then
      begin
        if WriteSize > FLASH_SIZE_128MBIT then //Память больше 128Мбит
        begin
          //4 байтная адресация
          BytesWrite := BytesWrite + UsbAsp25_Write32bitAddr($02, Address, datachunk, PageSize)
        end
        else //Память в пределах 128Мбит
          BytesWrite := BytesWrite + UsbAsp25_Write($02, Address, datachunk, PageSize);
      end else BytesWrite := BytesWrite + PageSize;
    end;

    if (not MainForm.MenuIgnoreBusyBit.Checked) and (not SkipPage) then  //Игнорировать проверку
      while UsbAsp25_Busy() do
      begin
        Application.ProcessMessages;
        if UserCancel then Exit;
      end;

    if (MainForm.MenuAutoCheck.Checked) and (WriteType = WT_PAGE) then
    begin
	  
      if WriteSize > FLASH_SIZE_128MBIT then
        UsbAsp25_Read32bitAddr($03, Address, datachunk2, PageSize)
      else
        UsbAsp25_Read($03, Address, datachunk2, PageSize);
		  
      for i:=0 to PageSize-1 do
        if DataChunk2[i] <> DataChunk[i] then
        begin
          LogPrint(STR_VERIFY_ERROR+IntToHex(Address+i, 8));
          MainForm.ProgressBar.Position := 0;
          Exit;
        end;
    end;

    Inc(Address, PageSize);
    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
    Application.ProcessMessages;

    if UserCancel then Break;
  end;

  if WriteSize > FLASH_SIZE_128MBIT then UsbAsp25_EX4B();
  UsbAsp25_Wrdi(); //Для sst

  if BytesWrite <> WriteSize then
    LogPrint(STR_WRONG_BYTES_WRITE)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;


function WriteFlash25NAND_SingleChunk(var RomStream: TMemoryStream;
                                       StartPage: cardinal;
                                       DataSize: cardinal;
                                       PageSize: word;
                                       PageOffsetForProgress: cardinal = 0): boolean;
var
  SPI_NAND_TOTAL_PAGE_SIZE: integer;
  DataChunk: array of byte;
  BytesWrite: cardinal;
  PageAddr, TotalPages: cardinal;
  FileOffsetInStream: Int64;
  BytesToReadThisPage: cardinal;
  i: integer;
begin
  Result := False;
  if (DataSize = 0) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    Exit;
  end;

  SPI_NAND_TOTAL_PAGE_SIZE := PageSize + CurrentICParam.Spare;
  SetLength(DataChunk, SPI_NAND_TOTAL_PAGE_SIZE);
  TotalPages := (DataSize + PageSize - 1) div PageSize;

  LogPrint('Writing chunk: Page ' + IntToStr(StartPage) + '..' +
           IntToStr(StartPage + TotalPages - 1) + ' (' + IntToStr(DataSize) + ' bytes)');

  BytesWrite := 0;
  RomStream.Position := 0;

  for PageAddr := 0 to TotalPages - 1 do
  begin
    // Reset buffer
    FillByte(DataChunk[0], SPI_NAND_TOTAL_PAGE_SIZE, $FF);

    // Đọc dữ liệu page từ stream
    FileOffsetInStream := Int64(PageAddr) * PageSize;
    if FileOffsetInStream < RomStream.Size then
    begin
      BytesToReadThisPage := Min(PageSize, RomStream.Size - FileOffsetInStream);
      RomStream.Position := FileOffsetInStream;
      RomStream.ReadBuffer(DataChunk[0], BytesToReadThisPage);
      // Phần còn lại (nếu có) giữ nguyên là $FF
    end;

    // Ghi page vào chip
    if UsbAsp25NAND_WritePage(StartPage + PageAddr, DataChunk, PageSize, SPI_NAND_TOTAL_PAGE_SIZE) <> PageSize then
    begin
      LogPrint('Error writing page ' + IntToStr(StartPage + PageAddr));
      Exit;
    end;

    Inc(BytesWrite, PageSize);
    if BytesWrite >= DataSize then Break;

    // Cập nhật progress
    MainForm.ProgressBar.Position := PageOffsetForProgress + PageAddr + 1;
    if (PageAddr mod 100 = 0) then
      Application.ProcessMessages;
    if UserCancel then
    begin
      LogPrint('Write cancelled by user at page ' + IntToStr(StartPage + PageAddr));
      Exit;
    end;
  end;

  if BytesWrite < DataSize then
  begin
    LogPrint('Chunk write incomplete: Written ' + IntToStr(BytesWrite) + '/' + IntToStr(DataSize));
    Exit;
  end;

  LogPrint('Chunk write successful');
  Result := True;
end;

function WriteFlash25NAND_Chunked(StartPage: cardinal;
                                  TotalDataSize: cardinal;
                                  PageSize: word;
                                  ShowProgress: boolean = True): boolean;
const
  CHUNK_SIZE = 134217728; // 128 MB
var
  ChunkCount: integer;
  ChunkIndex: integer;
  CurrentChunkSize: cardinal;
  FileOffsetStart: Int64;
  PagesPerChunk: cardinal;
  CurrentStartPage: cardinal;
  PageOffsetForProgress: cardinal;
  TotalPages: cardinal;
begin
  Result := False;
  if TotalDataSize = 0 then
  begin
    LogPrint('Error: No data to write');
    Exit;
  end;

  TotalPages := (TotalDataSize + PageSize - 1) div PageSize;
  ChunkCount := (TotalDataSize + CHUNK_SIZE - 1) div CHUNK_SIZE;

  if ShowProgress then
  begin
    MainForm.ProgressBar.Max := TotalPages;
    MainForm.ProgressBar.Position := 0;
  end;

  LogPrint('NAND Write - Chunked mode');
  LogPrint('Total pages: ' + IntToStr(TotalPages) + ', Chunks: ' + IntToStr(ChunkCount));

  for ChunkIndex := 0 to ChunkCount - 1 do
  begin
    FileOffsetStart := Int64(ChunkIndex) * CHUNK_SIZE;
    CurrentChunkSize := Min(CHUNK_SIZE, TotalDataSize - FileOffsetStart);
    PagesPerChunk := (CurrentChunkSize + PageSize - 1) div PageSize;
    CurrentStartPage := StartPage + (ChunkIndex * PagesPerChunk);
    PageOffsetForProgress := ChunkIndex * PagesPerChunk;

    // Load chunk vào RomF
    RomF.Clear;
    RomF.Size := 0;
    try
      MainForm.MPHexEditorEx.SaveRangeToStream(RomF, FileOffsetStart, CurrentChunkSize);
      if RomF.Size <> CurrentChunkSize then
      begin
        LogPrint('Error: Failed to load chunk ' + IntToStr(ChunkIndex));
        Exit;
      end;
    except
      on E: Exception do
      begin
        LogPrint('Error loading chunk: ' + E.Message);
        Exit;
      end;
    end;

    RomF.Position := 0;
    if not WriteFlash25NAND_SingleChunk(RomF, CurrentStartPage, RomF.Size, PageSize, PageOffsetForProgress) then
    begin
      LogPrint('Write failed at chunk ' + IntToStr(ChunkIndex + 1));
      Exit;
    end;

    if UserCancel then
    begin
      LogPrint('Write cancelled during chunking');
      Exit;
    end;
  end;

  Result := True;
end;


// Cập nhật hàm WriteFlash25NAND
function WriteFlash25NAND(StartPage: cardinal;
                          DataSize: cardinal;
                          PageSize: word): boolean;
  begin
    if DataSize < 67108864 then // < 64 MB
    begin

      //Result := WriteFlash25NAND_SingleChunk(RomStream, StartPage, DataSize, PageSize);
    end
    else
    begin
      // Dùng chunked
      Result := WriteFlash25NAND_Chunked(StartPage, DataSize, PageSize, True);
    end;
end;


procedure WriteFlash95(var RomStream: TMemoryStream; StartAddress, WriteSize: cardinal; PageSize: word; ChipSize: integer);
var
  DataChunk: array[0..2047] of byte;
  DataChunk2: array[0..2047] of byte;
  Address, BytesWrite: cardinal;
  PageSizeTemp: word;
  i: integer;
begin
  if (WriteSize = 0) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  if MainForm.MenuAutoCheck.Checked then
    LogPrint(STR_WRITING_FLASH_WCHK) else
      LogPrint(STR_WRITING_FLASH);

  PageSizeTemp := PageSize;
  BytesWrite := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := WriteSize div PageSize;

  while (Address-StartAddress) < WriteSize do
  begin
    UsbAsp95_WREN();

    //Determines first page buffer size to prevent buffer "rolls over" on address boundary
        if (StartAddress > 0) and (Address = StartAddress) and (PageSize > 1) then
           PageSize := (ChipSize - StartAddress) mod PageSize else
              PageSize := PageSizeTemp;

    if (WriteSize - (Address-StartAddress)) < PageSize then PageSize := (WriteSize - (Address-StartAddress));
    RomStream.ReadBuffer(DataChunk, PageSize);

    BytesWrite := BytesWrite + UsbAsp95_Write(ChipSize, Address, datachunk, PageSize);

    if not MainForm.MenuIgnoreBusyBit.Checked then  //Игнорировать проверку
      while UsbAsp25_Busy() do
      begin
        Application.ProcessMessages;
        if UserCancel then Exit;
      end;

    if MainForm.MenuAutoCheck.Checked then
    begin
      UsbAsp95_Read(ChipSize, Address, datachunk2, PageSize);
      for i:=0 to PageSize-1 do
        if DataChunk2[i] <> DataChunk[i] then
        begin
          LogPrint(STR_VERIFY_ERROR+IntToHex(Address+i, 8));
          MainForm.ProgressBar.Position := 0;
          Exit;
        end;
    end;

    Inc(Address, PageSize);
    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
    Application.ProcessMessages;
    if UserCancel then Break;
  end;

  if BytesWrite <> WriteSize then
    LogPrint(STR_WRONG_BYTES_WRITE)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;

procedure EraseEEPROM25(StartAddress, WriteSize: cardinal; PageSize: word; ChipSize: integer);
var
  DataChunk: array[0..2047] of byte;
  DataChunk2: array[0..2047] of byte;
  Address, BytesWrite: cardinal;
  i: integer;
begin
  if (StartAddress >= WriteSize) or (WriteSize = 0) {or (PageSize > WriteSize)} then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  BytesWrite := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := WriteSize div PageSize;

  while Address < WriteSize do
  begin
    UsbAsp95_WREN();

    if (WriteSize - Address) < PageSize then PageSize := (WriteSize - Address);

    FillByte(DataChunk, PageSize, $FF);

    BytesWrite := BytesWrite + UsbAsp95_Write(ChipSize, Address, datachunk, PageSize);

    if not MainForm.MenuIgnoreBusyBit.Checked then  //Игнорировать проверку
      while UsbAsp25_Busy() do
      begin
        Application.ProcessMessages;
        if UserCancel then Exit;
      end;

    if MainForm.MenuAutoCheck.Checked then
    begin
      UsbAsp95_Read(ChipSize, Address, datachunk2, PageSize);
      for i:=0 to PageSize-1 do
        if DataChunk2[i] <> DataChunk[i] then
        begin
          LogPrint(STR_VERIFY_ERROR+IntToHex(Address+i, 8));
          MainForm.ProgressBar.Position := 0;
          Exit;
        end;
    end;

    Inc(Address, PageSize);
    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
    Application.ProcessMessages;

    if UserCancel then Break;
  end;

  if BytesWrite <> WriteSize then
    LogPrint(STR_WRONG_BYTES_WRITE)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;

function EraseFlashKB(chipsize: longword; pagesize: word): integer;
var
  i: integer;
  busy: boolean;
begin
  MainForm.ProgressBar.Max := chipsize div pagesize;

  UsbAspMulti_EnableEDI();
  UsbAspMulti_WriteReg($FEA7, $A4); //en write

  for i:= 0 to (chipsize div pagesize)-1 do
  begin
    UsbAspMulti_ErasePage(i * pagesize);
    //busy
    repeat
      if UserCancel then Exit;
      busy := UsbAspMulti_Busy();
    until busy = false;

    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
  end;

  MainForm.ProgressBar.Position := 0;
end;

procedure WriteFlashKB(var RomStream: TMemoryStream; StartAddress, WriteSize: cardinal; PageSize: word);
var
  DataChunk: array[0..2047] of byte;
  DataChunk2: array[0..2047] of byte;
  Address, BytesWrite: cardinal;
  i: integer;
  busy: boolean;
  SkipPage: boolean = false;
begin
  if (StartAddress >= WriteSize) or (WriteSize = 0) {or (PageSize > WriteSize)} then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  if MainForm.MenuAutoCheck.Checked then
    LogPrint(STR_WRITING_FLASH_WCHK) else
      LogPrint(STR_WRITING_FLASH);

  BytesWrite := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := WriteSize div PageSize;

  UsbAspMulti_EnableEDI();
  UsbAspMulti_WriteReg($FEA7, $A4); //en write

  while Address < WriteSize do
  begin

    //if (WriteSize - Address) < PageSize then PageSize := (WriteSize - Address);
    RomStream.ReadBuffer(DataChunk, PageSize);


    //Если страница вся 00 то не пишем ее
    if MainForm.MenuSkipFF.Checked then
    begin
      SkipPage := True;
      for i:=0 to PageSize-1 do
        if DataChunk[i] <> $00 then
        begin
          SkipPage := False;
          Break;
        end;
    end;

    if not SkipPage then
      UsbAspMulti_WritePage(Address, datachunk);

    //busy
    repeat
      if UserCancel then Exit;
      busy := UsbAspMulti_Busy();
    until busy = false;

    BytesWrite := BytesWrite + PageSize;

     if (MainForm.MenuAutoCheck.Checked) then
      begin
        for i:=0 to PageSize-1 do
        begin
          UsbAspMulti_Read(Address+i, DataChunk2[0]);
          if DataChunk2[0] <> DataChunk[i] then
          begin
            LogPrint(STR_VERIFY_ERROR+IntToHex(Address+i, 8));
            MainForm.ProgressBar.Position := 0;
            Exit;
          end;
        end;
      end;

    Inc(Address, PageSize);
    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
    Application.ProcessMessages;
    if UserCancel then Exit;
  end;

  if BytesWrite <> WriteSize then
    LogPrint(STR_WRONG_BYTES_WRITE)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;

procedure WriteFlash45(var RomStream: TMemoryStream; StartAddress, ChipSize: cardinal; PageSize: word; WriteType: integer);
var
  DataChunk: array[0..2047] of byte;
  DataChunk2: array[0..2047] of byte;
  PageAddress, BytesWrite: cardinal;
  i: integer;
begin
  if (StartAddress >= ChipSize) or (ChipSize = 0) or (PageSize > ChipSize) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  if MainForm.MenuAutoCheck.Checked then
    LogPrint(STR_WRITING_FLASH_WCHK) else
      LogPrint(STR_WRITING_FLASH);

  BytesWrite := 0;
  PageAddress := StartAddress;
  MainForm.ProgressBar.Max := ChipSize div PageSize;

  while PageAddress < ChipSize div PageSize do
  begin
    //UsbAsp45_WREN(hUSBDev);
    RomStream.ReadBuffer(DataChunk, PageSize);

    if WriteType = WT_PAGE then
      BytesWrite := BytesWrite + UsbAsp45_Write(PageAddress, datachunk, PageSize);

    while UsbAsp45_Busy() do
    begin
      Application.ProcessMessages;
      if UserCancel then Exit;
    end;

    if MainForm.MenuAutoCheck.Checked then
    begin
      UsbAsp45_Read(PageAddress, datachunk2, PageSize);
      for i:=0 to PageSize-1 do
        if DataChunk2[i] <> DataChunk[i] then
        begin
          LogPrint(STR_VERIFY_ERROR+IntToHex((PageAddress*PageSize )+i, 8));
          Exit;
        end;
    end;

    Inc(PageAddress, 1);
    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
    Application.ProcessMessages;
    if UserCancel then Break;
  end;

  if BytesWrite <> ChipSize then
    LogPrint(STR_WRONG_BYTES_WRITE)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;

procedure ReadFlash25(var RomStream: TMemoryStream; StartAddress, ChipSize: cardinal);
const
  FLASH_SIZE_128MBIT = 16777216;
var
  ChunkSize: Word;
  BytesRead: integer;
  DataChunk: array[0..65534] of byte;
  Address: cardinal;
begin
  if (StartAddress >= ChipSize) or (ChipSize = 0) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  if ASProgrammer.Current_HW = CHW_FT232H then
    ChunkSize := 16787 else
  if ASProgrammer.Current_HW = CHW_CH347 then
    ChunkSize := SizeOf(DataChunk)
  else
    ChunkSize := 2048;



  if ChunkSize > ChipSize then ChunkSize := ChipSize;

  LogPrint(STR_READING_FLASH);
  BytesRead := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := ChipSize div ChunkSize;

  RomStream.Clear;

  if ChipSize > FLASH_SIZE_128MBIT then UsbAsp25_EN4B();

  while Address < ChipSize do
  begin
    if ChunkSize > (ChipSize - Address) then ChunkSize := ChipSize - Address;

    if ChipSize > FLASH_SIZE_128MBIT then
      BytesRead := BytesRead + UsbAsp25_Read32bitAddr($03, Address, datachunk, ChunkSize)
    else
      BytesRead := BytesRead + UsbAsp25_Read($03, Address, datachunk, ChunkSize);

    RomStream.WriteBuffer(datachunk, chunksize);
    Inc(Address, ChunkSize);

    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
    Application.ProcessMessages;

    if UserCancel then Break;
  end;

  if ChipSize > FLASH_SIZE_128MBIT then UsbAsp25_EX4B();

  if BytesRead <> ChipSize then
    LogPrint(STR_WRONG_BYTES_READ)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;


procedure ReadFlash25NAND_SingleChunk(var RomStream: TMemoryStream;
                                       StartPage: cardinal;
                                       DataSize: cardinal;
                                       PageOffsetForProgress: cardinal = 0);
var
  SPI_NAND_TOTAL_PAGE_SIZE: integer;
  DataChunk: array of byte;
  BytesRead, i: integer;
  PageAddr: cardinal;
  PageCount: cardinal;
  PageDataSize: integer;
  ChipPageAddr: cardinal;
  GlobalPageIndex: cardinal;
begin
  if DataSize = 0 then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    Exit;
  end;

  PageDataSize := CurrentICParam.Page;
  SPI_NAND_TOTAL_PAGE_SIZE := PageDataSize + CurrentICParam.Spare;
  SetLength(DataChunk, SPI_NAND_TOTAL_PAGE_SIZE);
  PageCount := (DataSize + PageDataSize - 1) div PageDataSize;

  LogPrint('Reading NAND chunk: Page ' + IntToStr(StartPage) + '..' +
           IntToStr(StartPage + PageCount - 1) +
           ' (' + IntToStr(DataSize) + ' bytes, ' + IntToStr(PageCount) + ' pages)');

  BytesRead := 0;
  RomStream.Clear;

  for PageAddr := 0 to PageCount - 1 do
  begin
    ChipPageAddr := StartPage + PageAddr;
    GlobalPageIndex := PageOffsetForProgress + PageAddr;

    if UsbAsp25NAND_ReadPageAuto(ChipPageAddr, DataChunk[0], SPI_NAND_TOTAL_PAGE_SIZE) <> SPI_NAND_TOTAL_PAGE_SIZE then
    begin
      LogPrint('Error reading page ' + IntToStr(ChipPageAddr));
      Exit;
    end;

    RomStream.WriteBuffer(DataChunk[0], PageDataSize);
    Inc(BytesRead, PageDataSize);
    if BytesRead >= DataSize then Break;

    MainForm.ProgressBar.Position := GlobalPageIndex + 1;
    if (PageAddr mod 100 = 0) then
      Application.ProcessMessages;
    if UserCancel then
    begin
      LogPrint('Read cancelled by user at page ' + IntToStr(ChipPageAddr));
      Exit;
    end;
  end;

  if BytesRead <> DataSize then
    LogPrint('Read incomplete: expected ' + IntToStr(DataSize) + ', read ' + IntToStr(BytesRead));
end;


procedure ReadFlash25NAND_Chunked(StartPage: cardinal;
                                   TotalDataSize: cardinal;
                                   ShowProgress: boolean = True);
const
  CHUNK_SIZE = 134217728; // 128 MB
var
  ChunkCount, ChunkIndex: integer;
  CurrentChunkSize: cardinal;
  PagesPerChunk, CurrentStartPage: cardinal;
  PageDataSize, TotalPages: cardinal;
  PageOffsetForProgress: cardinal;
  TempStream: TMemoryStream;
begin
  if TotalDataSize = 0 then
  begin
    LogPrint('Error: No data to read');
    Exit;
  end;

  PageDataSize := CurrentICParam.Page;
  if PageDataSize = 0 then
  begin
    LogPrint('Error: Page size not configured');
    Exit;
  end;

  TotalPages := (TotalDataSize + PageDataSize - 1) div PageDataSize;
  ChunkCount := (TotalDataSize + CHUNK_SIZE - 1) div CHUNK_SIZE;

  LogPrint('======================================');
  LogPrint('NAND Read - Chunked Mode');
  LogPrint('Total size: ' + IntToStr(TotalDataSize) + ' bytes (' +
           IntToStr(TotalDataSize div (1024*1024)) + ' MB)');
  LogPrint('Chunk size: ' + IntToStr(CHUNK_SIZE div (1024*1024)) + ' MB');
  LogPrint('Total chunks: ' + IntToStr(ChunkCount));
  LogPrint('Total pages: ' + IntToStr(TotalPages));
  LogPrint('Start page: ' + IntToStr(StartPage));
  LogPrint('======================================');

  if ShowProgress then
  begin
    MainForm.ProgressBar.Max := TotalPages;
    MainForm.ProgressBar.Position := 0;
  end;

  // Đảm bảo RomF sạch trước khi bắt đầu
  RomF.Clear;
  RomF.Size := 0;

  for ChunkIndex := 0 to ChunkCount - 1 do
  begin
    CurrentChunkSize := Min(CHUNK_SIZE, TotalDataSize - Int64(ChunkIndex) * CHUNK_SIZE);
    PagesPerChunk := (CurrentChunkSize + PageDataSize - 1) div PageDataSize;
    CurrentStartPage := StartPage + (ChunkIndex * PagesPerChunk);
    PageOffsetForProgress := ChunkIndex * PagesPerChunk;

    LogPrint('--- Reading Chunk ' + IntToStr(ChunkIndex + 1) + '/' + IntToStr(ChunkCount) +
             ' (' + IntToStr(CurrentChunkSize) + ' bytes) ---');

    TempStream := TMemoryStream.Create;
    try
      ReadFlash25NAND_SingleChunk(TempStream, CurrentStartPage, CurrentChunkSize, PageOffsetForProgress);
      RomF.CopyFrom(TempStream, 0); // Gộp vào RomF
    finally
      TempStream.Free;
    end;

    if UserCancel then
    begin
      LogPrint('Read cancelled during chunking');
      Exit;
    end;
  end;

  LogPrint('======================================');
  LogPrint('ALL CHUNKS READ SUCCESSFULLY!');
  LogPrint('======================================');
end;

// Cập nhật hàm ReadFlash25NAND
procedure ReadFlash25NAND(var RomStream: TMemoryStream; StartPage, ChipSize: cardinal);
begin
  if ChipSize < 67108864 then // < 64 MB
  begin
    ReadFlash25NAND_SingleChunk(RomStream, StartPage, ChipSize, 0);
  end
  else
  begin
    // Dùng hàm chunked — lưu kết quả vào RomF
    ReadFlash25NAND_Chunked(StartPage, ChipSize, True);
    // RomStream là tham số đầu vào, nhưng ta không đọc vào nó trực tiếp
    // Trong main.pas, bạn đang gọi: ReadFlash25NAND(RomF, ...);
    // → Cần gán RomF.Position := 0 sau khi đọc xong
    RomStream.Position := 0; // Đảm bảo stream ở đầu nếu cần
  end;
end;


procedure ReadFlash95(var RomStream: TMemoryStream; StartAddress, ChipSize: cardinal);
var
  ChunkSize: Word;
  BytesRead: integer;
  DataChunk: array[0..2047] of byte;
  Address: cardinal;
begin
  if (StartAddress >= ChipSize) or (ChipSize = 0) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  ChunkSize := SizeOf(DataChunk);
  if ChunkSize > ChipSize then ChunkSize := ChipSize;

  LogPrint(STR_READING_FLASH);
  BytesRead := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := ChipSize div ChunkSize;

  RomStream.Clear;

  while Address < ChipSize do
  begin
    if ChunkSize > (ChipSize - Address) then ChunkSize := ChipSize - Address;

    BytesRead := BytesRead + UsbAsp95_Read(ChipSize, Address, datachunk, ChunkSize);
    RomStream.WriteBuffer(datachunk, chunksize);
    Inc(Address, ChunkSize);

    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
    Application.ProcessMessages;

    if UserCancel then Break;
  end;

  if BytesRead <> ChipSize then
    LogPrint(STR_WRONG_BYTES_READ)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;

procedure ReadFlash45(var RomStream: TMemoryStream; StartAddress, PageSize, ChipSize: cardinal);
var
  ChunkSize: Word;
  BytesRead: integer;
  DataChunk: array[0..2047] of byte;
  Address: cardinal;
begin
  if (StartAddress >= ChipSize) or (ChipSize = 0) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  ChunkSize := PageSize;
  if ChunkSize > ChipSize then ChunkSize := ChipSize;

  LogPrint(STR_READING_FLASH);
  BytesRead := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := ChipSize div ChunkSize;

  RomStream.Clear;

  while Address < ChipSize div ChunkSize do
  begin
    if ChunkSize > (ChipSize - Address) then ChunkSize := ChipSize - Address;

    BytesRead := BytesRead + UsbAsp45_Read(Address, datachunk, ChunkSize);
    RomStream.WriteBuffer(datachunk, chunksize);
    Inc(Address, 1);

    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
    Application.ProcessMessages;

    if UserCancel then Break;
  end;

  if BytesRead <> ChipSize then
    LogPrint(STR_WRONG_BYTES_READ)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;

procedure ReadFlashKB(var RomStream: TMemoryStream; StartAddress, ChipSize: cardinal);
var
  ChunkSize: byte;
  BytesRead: integer;
  DataChunk: byte;
  Address: cardinal;
begin
  if (StartAddress >= ChipSize) or (ChipSize = 0) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  ChunkSize := SizeOf(DataChunk);
  if ChunkSize > ChipSize then ChunkSize := ChipSize;

  LogPrint(STR_READING_FLASH);
  BytesRead := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := ChipSize div ChunkSize;

  UsbAspMulti_EnableEDI();

  RomStream.Clear;

  while Address < ChipSize do
  begin
    if ChunkSize > (ChipSize - Address) then ChunkSize := ChipSize - Address;

    BytesRead := BytesRead + UsbAspMulti_Read(Address, datachunk);
    RomStream.WriteBuffer(datachunk, chunksize);
    Inc(Address, ChunkSize);

    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
    Application.ProcessMessages;

    if UserCancel then Break;
  end;

  if BytesRead <> ChipSize then
    LogPrint(STR_WRONG_BYTES_READ)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;


procedure VerifyFlash25(var RomStream: TMemoryStream; StartAddress, DataSize: cardinal);
const
  FLASH_SIZE_128MBIT = 16777216;
var
  ChunkSize: Word;
  BytesRead, i: integer;
  DataChunk: array[0..16786] of byte;
  DataChunkFile: array[0..16786] of byte;
  Address: cardinal;
begin
  if (DataSize = 0) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  if ASProgrammer.Current_HW = CHW_FT232H then
    ChunkSize := SizeOf(DataChunk)
  else
    ChunkSize := 2048;

  if ChunkSize > DataSize then ChunkSize := DataSize;

  LogPrint(STR_VERIFY);
  BytesRead := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := DataSize div ChunkSize;

  if DataSize > FLASH_SIZE_128MBIT then UsbAsp25_EN4B();

  while (Address-StartAddress) < DataSize do
  begin
    if ChunkSize > (DataSize - (Address-StartAddress)) then ChunkSize := DataSize - (Address-StartAddress);

    if DataSize > FLASH_SIZE_128MBIT then
        BytesRead := BytesRead + UsbAsp25_Read32bitAddr($03, Address, datachunk, ChunkSize)
      else
        BytesRead := BytesRead + UsbAsp25_Read($03, Address, datachunk, ChunkSize);

    RomStream.ReadBuffer(DataChunkFile, ChunkSize);

    for i := 0 to ChunkSize -1 do
    if DataChunk[i] <> DataChunkFile[i] then
    begin
      LogPrint(STR_VERIFY_ERROR+IntToHex(Address+i, 8));
      MainForm.ProgressBar.Position := 0;
      Exit;
    end;

    Inc(Address, ChunkSize);

    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
    Application.ProcessMessages;

    if UserCancel then Break;
  end;

  if DataSize > FLASH_SIZE_128MBIT then UsbAsp25_EX4B();

  if (BytesRead <> DataSize) then
    LogPrint(STR_WRONG_BYTES_READ)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;


// Cập nhật hàm VerifyFlash25NAND
// ============================================================================
// VERIFY FLASH 25 NAND - Chia nhỏ thành chunks
// ============================================================================
 // ============================================================================
// HÀM CƠ BẢN - Verify 1 chunk đơn lẻ
// ============================================================================

function VerifyFlash25NAND_SingleChunk(var RomStream: TMemoryStream;
                                       StartPage: cardinal;
                                       DataSize: cardinal;
                                       ChunkIndex: integer;
                                       TotalPages: cardinal;
                                       PageOffsetForProgress: cardinal): boolean;
var
  SPI_NAND_TOTAL_PAGE_SIZE: integer;
  DataChunkChip: array of byte;
  DataChunkFile: array of byte;
  BytesRead, i: integer;
  PageAddr: cardinal;
  PageCount: cardinal;
  FileOffsetInStream: Int64;
  BytesToCompareThisPage: cardinal;
  PageDataSize: integer;
  ChipPageAddr: cardinal;
  GlobalPageIndex: cardinal;
begin
  Result := False;

  if (DataSize = 0) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    Exit;
  end;

  PageDataSize := CurrentICParam.Page;
  SPI_NAND_TOTAL_PAGE_SIZE := PageDataSize + CurrentICParam.Spare;

  SetLength(DataChunkChip, SPI_NAND_TOTAL_PAGE_SIZE);
  SetLength(DataChunkFile, PageDataSize);

  PageCount := (DataSize + PageDataSize - 1) div PageDataSize;

  LogPrint('Verifying chunk #' + IntToStr(ChunkIndex) +
           ': Page ' + IntToStr(StartPage) + '..' + IntToStr(StartPage + PageCount - 1) +
           ' (' + IntToStr(DataSize) + ' bytes, ' + IntToStr(PageCount) + ' pages)');

  BytesRead := 0;
  RomStream.Position := 0;

  for PageAddr := 0 to PageCount - 1 do
  begin
    ChipPageAddr := StartPage + PageAddr;
    GlobalPageIndex := PageOffsetForProgress + PageAddr;

    // 1. Đọc từ chip
    if UsbAsp25NAND_ReadPageAuto(ChipPageAddr, DataChunkChip[0], SPI_NAND_TOTAL_PAGE_SIZE) <> SPI_NAND_TOTAL_PAGE_SIZE then
    begin
      LogPrint('ERROR: Failed to read page ' + IntToStr(ChipPageAddr) + ' from chip');
      Exit;
    end;

    // 2. Đọc từ stream
    FileOffsetInStream := Int64(PageAddr) * PageDataSize;

    if FileOffsetInStream >= RomStream.Size then
    begin
      FillByte(DataChunkFile[0], PageDataSize, $FF);
      BytesToCompareThisPage := PageDataSize;
    end
    else
    begin
      RomStream.Position := FileOffsetInStream;
      BytesToCompareThisPage := Min(PageDataSize, RomStream.Size - FileOffsetInStream);
      RomStream.ReadBuffer(DataChunkFile[0], BytesToCompareThisPage);

      if BytesToCompareThisPage < PageDataSize then
        FillByte(DataChunkFile[BytesToCompareThisPage], PageDataSize - BytesToCompareThisPage, $FF);
    end;

    // 3. So sánh
    for i := 0 to PageDataSize - 1 do
    begin
      if DataChunkChip[i] <> DataChunkFile[i] then
      begin
        LogPrint(STR_VERIFY_ERROR + IntToHex(ChipPageAddr * PageDataSize + i, 8) +
                 ' (Page ' + IntToStr(ChipPageAddr) + ', Offset +' + IntToHex(i, 4) +
                 ') Chip: $' + IntToHex(DataChunkChip[i], 2) +
                 ', File: $' + IntToHex(DataChunkFile[i], 2));
        MainForm.ProgressBar.Position := 0;
        Exit;
      end;
    end;

    Inc(BytesRead, BytesToCompareThisPage);
    if BytesRead >= DataSize then Break;

    MainForm.ProgressBar.Position := GlobalPageIndex + 1;

    if (PageAddr mod 100 = 0) then
      Application.ProcessMessages;

    if UserCancel then
    begin
      LogPrint('Verify cancelled by user at page ' + IntToStr(ChipPageAddr));
      Exit;
    end;
  end;

  if BytesRead < DataSize then
  begin
    LogPrint('Chunk #' + IntToStr(ChunkIndex) + ' - ' + STR_WRONG_BYTES_READ +
             ' Verified: ' + IntToStr(BytesRead) + ', Expected: ' + IntToStr(DataSize));
    Exit;
  end
  else
  begin
    LogPrint('Chunk #' + IntToStr(ChunkIndex) + ' - ' + STR_DONE);
    Result := True;
  end;
end;

// ============================================================================
// HÀM WRAPPER - Xử lý chunking và gọi verify
// Đây là hàm PUBLIC để gọi từ bên ngoài
// ============================================================================

 function VerifyFlash25NAND_Chunked(StartPage: cardinal;
                                   TotalDataSize: cardinal;
                                   ShowProgress: boolean = True;
                                   BlankCheck: boolean = False): boolean;
var
  ChunkSize: cardinal;
  ChunkCount: integer;
  ChunkIndex: integer;
  CurrentChunkSize: cardinal;
  FileOffsetStart: Int64;
  PagesPerChunk: cardinal;
  CurrentStartPage: cardinal;
  PageDataSize: integer;
  TotalPages: cardinal;
  PageOffsetForProgress: cardinal;
  TempStream: TMemoryStream;
  i: cardinal;
begin
  Result := False;
  if TotalDataSize = 0 then
  begin
    LogPrint('Error: No data to verify');
    Exit;
  end;

  PageDataSize := CurrentICParam.Page;
  if PageDataSize = 0 then
  begin
    LogPrint('Error: Page size not configured');
    Exit;
  end;

  // Cấu hình chunk
  ChunkSize := 134217728;  // 128 MB
  ChunkCount := (TotalDataSize + ChunkSize - 1) div ChunkSize;
  TotalPages := (TotalDataSize + PageDataSize - 1) div PageDataSize;

  LogPrint('======================================');
  if BlankCheck then
    LogPrint('NAND Blank Check - Chunked Mode')
  else
    LogPrint('NAND Verify - Chunked Mode');
  LogPrint('Total size: ' + IntToStr(TotalDataSize) + ' bytes (' +
           IntToStr(TotalDataSize div (1024*1024)) + ' MB)');
  LogPrint('Chunk size: ' + IntToStr(ChunkSize div (1024*1024)) + ' MB');
  LogPrint('Total chunks: ' + IntToStr(ChunkCount));
  LogPrint('Total pages: ' + IntToStr(TotalPages));
  LogPrint('Start page: ' + IntToStr(StartPage));
  LogPrint('======================================');

  if ShowProgress then
  begin
    MainForm.ProgressBar.Max := TotalPages;
    MainForm.ProgressBar.Position := 0;
  end;

  CurrentStartPage := StartPage;
  PageOffsetForProgress := 0;

  for ChunkIndex := 0 to ChunkCount - 1 do
  begin
    LogPrint('');
    LogPrint('--- Processing Chunk ' + IntToStr(ChunkIndex + 1) + '/' + IntToStr(ChunkCount) + ' ---');

    FileOffsetStart := Int64(ChunkIndex) * ChunkSize;
    if ChunkIndex = ChunkCount - 1 then
      CurrentChunkSize := TotalDataSize - FileOffsetStart
    else
      CurrentChunkSize := ChunkSize;

    // === XỬ LÝ BLANK CHECK ===
    if BlankCheck then
    begin
      TempStream := TMemoryStream.Create;
      try
        TempStream.Size := CurrentChunkSize;
        FillChar(TempStream.Memory^, CurrentChunkSize, $FF);
        TempStream.Position := 0;
      except
        on E: Exception do
        begin
          LogPrint('ERROR creating BlankCheck stream: ' + E.Message);
          TempStream.Free;
          Exit;
        end;
      end;
    end
    else
    begin
      // === XỬ LÝ FILE VERIFY ===
      TempStream := TMemoryStream.Create;
      try
        MainForm.MPHexEditorEx.SaveRangeToStream(TempStream, FileOffsetStart, CurrentChunkSize);
        if TempStream.Size <> CurrentChunkSize then
        begin
          LogPrint('ERROR: Expected ' + IntToStr(CurrentChunkSize) +
                   ', got ' + IntToStr(TempStream.Size) + ' bytes from hex editor');
          TempStream.Free;
          Exit;
        end;
        TempStream.Position := 0;
      except
        on E: Exception do
        begin
          LogPrint('ERROR loading chunk from hex editor: ' + E.Message);
          TempStream.Free;
          Exit;
        end;
      end;
    end;

    // Tính start page cho chunk này
    PagesPerChunk := (ChunkSize + PageDataSize - 1) div PageDataSize; // round up
    CurrentStartPage := StartPage + (ChunkIndex * PagesPerChunk);
    PageOffsetForProgress := ChunkIndex * PagesPerChunk;

    // Gọi verify single chunk
    if not VerifyFlash25NAND_SingleChunk(TempStream, CurrentStartPage, TempStream.Size,
                                         ChunkIndex, TotalPages, PageOffsetForProgress) then
    begin
      LogPrint('');
      LogPrint('======================================');
      LogPrint('VERIFY FAILED AT CHUNK ' + IntToStr(ChunkIndex + 1) + '/' + IntToStr(ChunkCount));
      LogPrint('======================================');
      TempStream.Free;
      if ShowProgress then MainForm.ProgressBar.Position := 0;
      Exit;
    end;

    TempStream.Free;

    if UserCancel then
    begin
      LogPrint('Verify cancelled by user');
      if ShowProgress then MainForm.ProgressBar.Position := 0;
      Exit;
    end;
  end;

  LogPrint('');
  LogPrint('======================================');
  if BlankCheck then
    LogPrint('BLANK CHECK SUCCESS: Chip is fully erased ($FF)')
  else
    LogPrint('ALL CHUNKS VERIFIED SUCCESSFULLY!');
  LogPrint('======================================');

  if ShowProgress then
    MainForm.ProgressBar.Position := TotalPages;
  Result := True;
end;

// ============================================================================
// HÀM WRAPPER ĐƠN GIẢN - Cho compatibility với code cũ
// Hàm này gọi từ ButtonWriteClick (MenuAutoCheck.Checked)
// ============================================================================

function VerifyFlash25NAND(var RomStream: TMemoryStream;
                           StartPage: cardinal;
                           DataSize: cardinal): boolean;
begin
  // Nếu data nhỏ (< 64MB), verify trực tiếp không cần chunk
  if DataSize < 67108864 then
  begin
    Result := VerifyFlash25NAND_SingleChunk(RomStream, StartPage, DataSize,
                                            0, // ChunkIndex = 0
                                            (DataSize + CurrentICParam.Page - 1) div CurrentICParam.Page, // TotalPages
                                            0); // PageOffset = 0
    Exit;
  end;

  // Data lớn: dùng chunked verify
  LogPrint('Data size large (' + IntToStr(DataSize div (1024*1024)) + ' MB), using chunked verify');
  Result := VerifyFlash25NAND_Chunked(StartPage, DataSize, True);
end;

procedure VerifyFlash95(var RomStream: TMemoryStream; StartAddress, DataSize, ChipSize: cardinal);
var
  ChunkSize: Word;
  BytesRead, i: integer;
  DataChunk: array[0..2047] of byte;
  DataChunkFile: array[0..2047] of byte;
  Address: cardinal;
begin
  if (DataSize = 0) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  ChunkSize := SizeOf(DataChunk);
  if ChunkSize > DataSize then ChunkSize := DataSize;

  LogPrint(STR_VERIFY);
  BytesRead := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := DataSize div ChunkSize;

  while (Address-StartAddress) < DataSize do
  begin
    if ChunkSize > (DataSize - (Address-StartAddress)) then ChunkSize := DataSize - (Address-StartAddress);

    BytesRead := BytesRead + UsbAsp95_Read(ChipSize, Address, datachunk, ChunkSize);
    RomStream.ReadBuffer(DataChunkFile, ChunkSize);

    for i := 0 to ChunkSize -1 do
    if DataChunk[i] <> DataChunkFile[i] then
    begin
      LogPrint(STR_VERIFY_ERROR+IntToHex(Address+i, 8));
      MainForm.ProgressBar.Position := 0;
      Exit;
    end;

    Inc(Address, ChunkSize);

    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
    Application.ProcessMessages;

    if UserCancel then Break;
  end;

  if (BytesRead <> DataSize) then
    LogPrint(STR_WRONG_BYTES_READ)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;

procedure VerifyFlash45(var RomStream: TMemoryStream; StartAddress, PageSize, ChipSize: cardinal);
var
  ChunkSize: Word;
  BytesRead, i: integer;
  DataChunk: array[0..2047] of byte;
  DataChunkFile: array[0..2047] of byte;
  PageAddress: cardinal;
begin
  if (StartAddress >= ChipSize) or (ChipSize = 0) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  ChunkSize := PageSize;
  if ChunkSize > ChipSize then ChunkSize := ChipSize;

  LogPrint(STR_VERIFY);
  BytesRead := 0;
  PageAddress := StartAddress;
  MainForm.ProgressBar.Max := ChipSize div ChunkSize;

  while PageAddress < ChipSize div ChunkSize do
  begin
    //if ChunkSize > (ChipSize - Address) then ChunkSize := ChipSize - Address;

    BytesRead := BytesRead + UsbAsp45_Read(PageAddress, datachunk, ChunkSize);
    RomStream.ReadBuffer(DataChunkFile, ChunkSize);

    for i := 0 to ChunkSize -1 do
    if DataChunk[i] <> DataChunkFile[i] then
    begin
      LogPrint(STR_VERIFY_ERROR+IntToHex((PageAddress*ChunkSize)+i, 8));
      MainForm.ProgressBar.Position := 0;
      Exit;
    end;

    Inc(PageAddress, 1);

    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
    Application.ProcessMessages;

    if UserCancel then Break;
  end;

  if (BytesRead <> ChipSize) then
    LogPrint(STR_WRONG_BYTES_READ)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;

procedure VerifyFlashMW(var RomStream: TMemoryStream; AddrBitLen: byte; StartAddress, ChipSize: cardinal);
var
  ChunkSize: Word;
  BytesRead, i: integer;
  DataChunk: array[0..2047] of byte;
  DataChunkFile: array[0..2047] of byte;
  Address: cardinal;
begin
  if (StartAddress >= ChipSize) or (ChipSize = 0) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  ChunkSize := 2;
  if ChunkSize > ChipSize then ChunkSize := ChipSize;

  LogPrint(STR_VERIFY);
  BytesRead := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := ChipSize div ChunkSize;

  while Address < ChipSize div 2 do
  begin
    BytesRead := BytesRead + UsbAspMW_Read(AddrBitLen, Address, datachunk, ChunkSize);
    RomStream.ReadBuffer(DataChunkFile, ChunkSize);

    for i := 0 to ChunkSize -1 do
    if DataChunk[i] <> DataChunkFile[i] then
    begin
      LogPrint(STR_VERIFY_ERROR+IntToHex(Address+i, 8));
      MainForm.ProgressBar.Position := 0;
      Exit;
    end;

    Inc(Address, ChunkSize div 2);

    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 2;
    Application.ProcessMessages;
    if UserCancel then Break;
  end;

  if (BytesRead <> ChipSize) then
    LogPrint(STR_WRONG_BYTES_READ)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;

procedure VerifyFlashKB(var RomStream: TMemoryStream; StartAddress, ChipSize: cardinal);
var
  ChunkSize: byte;
  BytesRead: integer;
  DataChunk: byte;
  DataChunkFile: byte;
  Address: cardinal;
begin
  if (StartAddress >= ChipSize) or (ChipSize = 0) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  ChunkSize := SizeOf(DataChunk);
  if ChunkSize > ChipSize then ChunkSize := ChipSize;

  LogPrint(STR_VERIFY);
  BytesRead := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := ChipSize div ChunkSize;

  UsbAspMulti_EnableEDI();
  UsbAspMulti_WriteReg($FEAD, $08); //en flash

  //RomStream.Clear;

  while Address < ChipSize do
  begin
    if ChunkSize > (ChipSize - Address) then ChunkSize := ChipSize - Address;

    BytesRead := BytesRead + UsbAspMulti_Read(Address, datachunk);
    RomStream.ReadBuffer(DataChunkFile, ChunkSize);

    if DataChunk <> DataChunkFile then
    begin
      LogPrint(STR_VERIFY_ERROR+IntToHex(Address, 8));
      MainForm.ProgressBar.Position := 0;
      Exit;
    end;

    Inc(Address, ChunkSize);

    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
    Application.ProcessMessages;

    if UserCancel then Break;
  end;

  if BytesRead <> ChipSize then
    LogPrint(STR_WRONG_BYTES_READ)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;

procedure ReadFlashI2C(var RomStream: TMemoryStream; StartAddress, ChipSize: cardinal; ChunkSize: Word; DevAddr: byte);
var
  BytesRead: integer;
  DataChunk: array[0..255] of byte;
  Address: cardinal;
begin
  if ChipSize = 0 then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  if ChunkSize > SizeOf(DataChunk) then ChunkSize := SizeOf(DataChunk);
  if ChunkSize < 1 then ChunkSize := 1;
  if ChunkSize > ChipSize then ChunkSize := ChipSize;

  LogPrint(STR_READING_FLASH);
  BytesRead := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := ChipSize div ChunkSize;

  RomStream.Clear;

  while Address < ChipSize do
  begin
    if ChunkSize > (ChipSize - Address) then ChunkSize := ChipSize - Address;

    BytesRead := BytesRead + UsbAspI2C_Read(DevAddr, MainForm.ComboAddrType.ItemIndex, Address, datachunk, ChunkSize);
    RomStream.WriteBuffer(DataChunk, ChunkSize);
    Inc(Address, ChunkSize);

    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
    Application.ProcessMessages;
    if UserCancel then Break;
  end;

  if BytesRead <> ChipSize then
    LogPrint(STR_WRONG_BYTES_READ)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;

procedure WriteFlashI2C(var RomStream: TMemoryStream; StartAddress, WriteSize: cardinal; PageSize: word; DevAddr: byte);
var
  DataChunk: array[0..2047] of byte;
  Address, BytesWrite: cardinal;
  PageSizeTemp: word;
begin
  if {(StartAddress >= WriteSize) or} (WriteSize = 0) {or (PageSize > WriteSize)} then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  PageSizeTemp := PageSize;
  LogPrint(STR_WRITING_FLASH);
  BytesWrite := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := WriteSize div PageSize;

  while (Address-StartAddress) < WriteSize do
  begin
    //Determines first page buffer size to prevent buffer "rolls over" on address boundary
    if (StartAddress > 0) and (Address = StartAddress) and (PageSize > 1) then
       PageSize := (StrToInt(MainForm.ComboChipSize.Text) - StartAddress) mod PageSize else
           PageSize := PageSizeTemp;

    if (WriteSize - (Address-StartAddress)) < PageSize then PageSize := (WriteSize - (Address-StartAddress));

    RomStream.ReadBuffer(DataChunk, PageSize);
    BytesWrite := BytesWrite + UsbAspI2C_Write(DevAddr, MainForm.ComboAddrType.ItemIndex, Address, datachunk, PageSize);
    Inc(Address, PageSize);

    while UsbAspI2C_BUSY(DevAddr) do
    begin
      Application.ProcessMessages;
      if UserCancel then Exit;
    end; 

    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
    Application.ProcessMessages;
    if UserCancel then Break;
  end;

  if BytesWrite <> WriteSize then
    LogPrint(STR_WRONG_BYTES_WRITE)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;

procedure EraseFlashI2C(StartAddress, WriteSize: cardinal; PageSize: word; DevAddr: byte);
var
  DataChunk: array[0..2047] of byte;
  Address, BytesWrite: cardinal;
begin
  if (StartAddress >= WriteSize) or (WriteSize = 0) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  LogPrint(STR_ERASING_FLASH);
  BytesWrite := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := WriteSize div PageSize;

  while Address < WriteSize do
  begin
    if (WriteSize - Address) < PageSize then PageSize := (WriteSize - Address);
    FillByte(DataChunk, PageSize, $FF);
    BytesWrite := BytesWrite + UsbAspI2C_Write(DevAddr, MainForm.ComboAddrType.ItemIndex, Address, datachunk, PageSize);
    Inc(Address, PageSize);

    while UsbAspI2C_BUSY(DevAddr) do
    begin
      Application.ProcessMessages;
      if UserCancel then Exit;
    end; 

    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
    Application.ProcessMessages;
    if UserCancel then Break;
  end;

  if BytesWrite <> WriteSize then
    LogPrint(STR_WRONG_BYTES_WRITE)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;

procedure VerifyFlashI2C(var RomStream: TMemoryStream; StartAddress, DataSize: cardinal; ChunkSize: Word; DevAddr: byte);
var
  BytesRead, i: integer;
  DataChunk: array[0..2047] of byte;
  DataChunkFile: array[0..2047] of byte;
  Address: cardinal;
begin
  if (DataSize = 0) then
  begin
    LogPrint(STR_CHECK_SETTINGS);
    exit;
  end;

  if ChunkSize > SizeOf(DataChunk) then ChunkSize := SizeOf(DataChunk);
  if ChunkSize < 1 then ChunkSize := 1;
  if ChunkSize > DataSize then ChunkSize := DataSize;

  LogPrint(STR_VERIFY);
  BytesRead := 0;
  Address := StartAddress;
  MainForm.ProgressBar.Max := DataSize div ChunkSize;

  while (Address-StartAddress) < DataSize do
  begin
    if ChunkSize > (DataSize - (Address - StartAddress)) then ChunkSize := DataSize -(Address - StartAddress) ;

    BytesRead := BytesRead + UsbAspI2C_Read(DevAddr, MainForm.ComboAddrType.ItemIndex, Address, datachunk, ChunkSize);
    RomStream.ReadBuffer(DataChunkFile, ChunkSize);

    for i := 0 to ChunkSize -1 do
    if DataChunk[i] <> DataChunkFile[i] then
    begin
      LogPrint(STR_VERIFY_ERROR+IntToHex(Address+i, 8));
      MainForm.ProgressBar.Position := 0;
      Exit;
    end;

    Inc(Address, ChunkSize);

    MainForm.ProgressBar.Position := MainForm.ProgressBar.Position + 1;
    Application.ProcessMessages;
    if UserCancel then Break;
  end;

  if (BytesRead <> DataSize) then
    LogPrint(STR_WRONG_BYTES_READ)
  else
    LogPrint(STR_DONE);

  MainForm.ProgressBar.Position := 0;
end;

procedure SelectHW(programmer: THardwareList);
begin
  if programmer = CHW_USBASP then
  begin
    MainForm.MenuSPIClock.Visible:= true;
    MainForm.MenuCH347SPIClock.Visible:= false;
    MainForm.MenuAVRISPSPIClock.Visible:= false;
    MainForm.MenuArduinoSPIClock.Visible:= false;
    MainForm.MenuFT232SPIClock.Visible:= false;
    MainForm.MenuMicrowire.Enabled:= true;
    AsProgrammer.Current_HW := CHW_USBASP;
  end;

  if programmer = CHW_CH341 then
  begin
    MainForm.MenuSPIClock.Visible:= false;
    MainForm.MenuCH347SPIClock.Visible:= false;
    MainForm.MenuAVRISPSPIClock.Visible:= false;
    MainForm.MenuArduinoSPIClock.Visible:= false;
    MainForm.MenuFT232SPIClock.Visible:= false;
    MainForm.MenuMicrowire.Enabled:= false;
    AsProgrammer.Current_HW := CHW_CH341;
  end;

  if programmer = CHW_CH347 then
  begin
    MainForm.MenuCH347SPIClock.Visible:= true;
    MainForm.MenuSPIClock.Visible:= false;
    MainForm.MenuAVRISPSPIClock.Visible:= false;
    MainForm.MenuArduinoSPIClock.Visible:= false;
    MainForm.MenuFT232SPIClock.Visible:= false;
    MainForm.MenuMicrowire.Enabled:= false;
    AsProgrammer.Current_HW := CHW_CH347;
  end;

  if programmer = CHW_AVRISP then
  begin
    MainForm.MenuSPIClock.Visible:= false;
    MainForm.MenuCH347SPIClock.Visible:= false;
    MainForm.MenuAVRISPSPIClock.Visible:= true;
    MainForm.MenuArduinoSPIClock.Visible:= false;
    MainForm.MenuFT232SPIClock.Visible:= false;
    MainForm.MenuMicrowire.Enabled:= false;
    AsProgrammer.Current_HW := CHW_AVRISP;
  end;

  if programmer = CHW_ARDUINO then
  begin
    MainForm.MenuSPIClock.Visible:= false;
    MainForm.MenuCH347SPIClock.Visible:= false;
    MainForm.MenuAVRISPSPIClock.Visible:= false;
    MainForm.MenuArduinoSPIClock.Visible:= true;
    MainForm.MenuFT232SPIClock.Visible:= false;
    MainForm.MenuMicrowire.Enabled:= false;
    AsProgrammer.Current_HW := CHW_ARDUINO;
  end;

  if programmer = CHW_FT232H then
  begin
    MainForm.MenuFT232SPIClock.Visible:= true;
    MainForm.MenuCH347SPIClock.Visible:= false;
    MainForm.MenuSPIClock.Visible:= false;
    MainForm.MenuAVRISPSPIClock.Visible:= false;
    MainForm.MenuArduinoSPIClock.Visible:= false;
    MainForm.MenuMicrowire.Enabled:= false;
    AsProgrammer.Current_HW := CHW_FT232H;
  end;

end;

procedure LockControl;
begin
  MainForm.ButtonRead.Enabled := False;
  MainForm.ButtonWrite.Enabled := False;
  MainForm.ButtonVerify.Enabled := False;
  MainForm.ButtonReadID.Enabled := False;
  MainForm.ButtonBlock.Enabled := False;
  MainForm.ButtonErase.Enabled := False;
  MainForm.ButtonOpenHex.Enabled := False;
  MainForm.ButtonSaveHex.Enabled := False;

  MainForm.GroupChipSettings.Enabled := false;
  MainForm.MPHexEditorEx.Enabled := false;
end;

procedure UnlockControl;
begin
  MainForm.MPHexEditorEx.Enabled := true;
  MainForm.GroupChipSettings.Enabled := true;
  MainForm.ButtonRead.Enabled := True;
  MainForm.ButtonWrite.Enabled := True;
  MainForm.ButtonVerify.Enabled := True;
  MainForm.ButtonOpenHex.Enabled := True;
  MainForm.ButtonSaveHex.Enabled := True;
  MainForm.ButtonErase.Enabled := True;

  if MainForm.RadioSPI.Checked then
  begin
    MainForm.ButtonReadID.Enabled := True;
    if MainForm.ComboSPICMD.ItemIndex = SPI_CMD_KB then
      MainForm.ButtonBlock.Enabled := False
    else
      MainForm.ButtonBlock.Enabled := True;
  end;
end;

procedure TMainForm.ChipClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    findchip.SelectChip(chiplistfile, TMenuItem(Sender).Caption);
end;

procedure TMainForm.MPHexEditorExChange(Sender: TObject);
begin
  StatusBar.Panels.Items[0].Text := STR_SIZE+IntToStr(MPHexEditorEx.DataSize);
  if MPHexEditorEx.Modified then
    StatusBar.Panels.Items[1].Text := STR_CHANGED
  else
    StatusBar.Panels.Items[1].Text := '';
end;

procedure TMainForm.ComboItem1Click(Sender: TObject);
var
  CheckTemp: Boolean;
begin
  if MessageDlg('AsProgrammer', STR_COMBO_WARN, mtConfirmation, [mbYes, mbNo], 0)
    <> mrYes then Exit;

  if ButtonBlock.Enabled then
    ButtonBlockClick(Sender);
  if ButtonErase.Enabled then
    if ComboSPICMD.ItemIndex <> SPI_CMD_45 then  //Сами стирают страницу
      ButtonEraseClick(Sender);

  CheckTemp := MenuAutoCheck.Checked;
  MenuAutoCheck.Checked := True;

  ButtonWriteClick(Sender);

  MenuAutoCheck.Checked := CheckTemp;
end;




procedure TMainForm.MenuArduinoCOMPortClick(Sender: TObject);
begin
  Arduino_COMPort := InputBox('Arduino COMPort','',Arduino_COMPort);
  MainForm.MenuArduinoCOMPort.Caption := 'Arduino COMPort: '+Arduino_COMPort;
end;

procedure TMainForm.MenuCopyToClipClick(Sender: TObject);
begin
    MainForm.MPHexEditorEx.CBCopy;
end;

procedure TMainForm.MenuFindChipClick(Sender: TObject);
begin
  ChipSearchForm.EditSearch.Text:= '';
  ChipSearchForm.ListBoxChips.Items.Clear;
  ChipSearchForm.Show;
  ChipSearchForm.EditSearch.SetFocus;
end;

procedure TMainForm.MenuFindClick(Sender: TObject);
begin
  Search.SearchForm.Show;
end;

procedure TMainForm.MenuGotoOffsetClick(Sender: TObject);
var
  s : string;
  addr: integer;
begin
  s := InputBox(STR_GOTO_ADDR,'','');
  s := Trim(s);
  if IsNumber('$'+s)  then
  begin
    addr := StrToInt('$' + s);
    MainForm.MPHexEditorEx.SelStart := addr;
    MainForm.MPHexEditorEx.SelEnd := addr;
  end;
end;

procedure TMainForm.MenuHWCH341AClick(Sender: TObject);
begin
  SelectHW(CHW_CH341);
end;

procedure TMainForm.MenuHWCH347Click(Sender: TObject);
begin
  SelectHW(CHW_CH347);
end;

procedure TMainForm.MenuHWFT232HClick(Sender: TObject);
begin
  SelectHW(CHW_FT232H);
end;

procedure TMainForm.MenuHWUSBASPClick(Sender: TObject);
begin
  SelectHW(CHW_USBASP);
end;

procedure TMainForm.MenuHWAVRISPClick(Sender: TObject);
begin
  SelectHW(CHW_AVRISP);
end;

procedure TMainForm.MenuHWARDUINOClick(Sender: TObject);
begin
  SelectHW(CHW_ARDUINO);
end;

procedure TMainForm.MenuItemBenchmarkClick(Sender: TObject);
var
  buffer: array[0..2047] of byte;
  i, cycles: integer;
  t: TDateTime;
  timeval: integer;
  ms, sec, d: word;
begin
  ButtonCancel.Tag := 0;
  if not OpenDevice() then exit;
  EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked);
  LockControl();

  if (AsProgrammer.Current_HW = CHW_CH341) or (AsProgrammer.Current_HW = CHW_AVRISP) or (AsProgrammer.Current_HW = CHW_CH347)
    or (AsProgrammer.Current_HW = CHW_FT232H) then
    cycles := 256
  else
    cycles := 32;

  LogPrint('Benchmark read '+ IntToStr(SizeOf(buffer))+' bytes * '+ IntToStr(cycles) +' cycles');
  Application.ProcessMessages();
  TimeCounter := Time();

  for i:=1 to cycles do
  begin
    UsbAsp25_Read(0, 0, buffer, sizeof(buffer));
    Application.ProcessMessages;

    if UserCancel then Break;
  end;

  t :=  Time() - TimeCounter;
  DecodeDateTime(t, d, d, d, d, d, sec, ms);

  timeval := (sec * 1000) + ms;
  if timeval = 0 then timeval := 1;

  LogPrint(STR_TIME + TimeToStr(t)+' '+
    IntToStr( Trunc(((cycles*sizeof(buffer)) / timeval) * 1000)) +' bytes/s');

  LogPrint('Benchmark write '+ IntToStr(SizeOf(buffer))+' bytes * '+ IntToStr(cycles) +' cycles');
  Application.ProcessMessages();
  TimeCounter := Time();

  for i:=1 to cycles do
  begin
    UsbAsp25_Write(0, 0, buffer, sizeof(buffer));
    Application.ProcessMessages;

    if UserCancel then Break;
  end;

  t :=  Time() - TimeCounter;
  DecodeDateTime(t, d, d, d, d, d, sec, ms);

  timeval := (sec * 1000) + ms;
  if timeval = 0 then timeval := 1;

  LogPrint(STR_TIME + TimeToStr(t)+' '+
    IntToStr( Trunc(((cycles*sizeof(buffer)) / timeval) * 1000)) +' bytes/s');

  ExitProgMode25;
  AsProgrammer.Programmer.DevClose;
  UnlockControl();
end;

procedure TMainForm.MenuItemEditSregClick(Sender: TObject);
begin
  // Kiểm tra cả SPI_CMD_25 (NOR) và SPI_CMD_25_NAND
  if (MainForm.ComboSPICMD.ItemIndex = SPI_CMD_25) or
     (MainForm.ComboSPICMD.ItemIndex = SPI_CMD_25_NAND) then
  begin
    sregedit.sregeditForm.Show; // Mở form, logic trong sregeditForm sẽ xử lý UI
  end
  else
  begin
    // (Tùy chọn) Hiển thị thông báo nếu chức năng không hỗ trợ cho loại chip hiện tại
    // LogPrint('SREG/Feature Register Editor is not supported for this chip type.');
  end;
end;

procedure TMainForm.MenuItemLockFlashClick(Sender: TObject);
var
  sreg: byte;
begin
  try
  ButtonCancel.Tag := 0;
  if not OpenDevice() then exit;
  sreg:= 0;
  LockControl();
  EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked);

  if ComboSPICMD.ItemIndex = SPI_CMD_25 then
  begin
    UsbAsp25_ReadSR(sreg); //Читаем регистр
    LogPrint(STR_OLD_SREG+IntToBin(sreg, 8));

    sreg := %10011100; //
    UsbAsp25_WREN(); //Включаем разрешение записи
    UsbAsp25_WriteSR(sreg); //Устанавливаем регистр

    //Пока отлипнет ромка
    while UsbAsp25_Busy() do
    begin
      Application.ProcessMessages;
      if UserCancel then Exit;
    end;

    LogPrint(STR_NEW_SREG+IntToBin(sreg, 8));
  end;

  if ComboSPICMD.ItemIndex = SPI_CMD_95 then
  begin
    UsbAsp95_ReadSR(sreg); //Читаем регистр
    LogPrint(STR_OLD_SREG+IntToBin(sreg, 8));

    sreg := %10011100; //
    UsbAsp95_WREN(); //Включаем разрешение записи
    UsbAsp95_WriteSR(sreg); //Устанавливаем регистр

    //Пока отлипнет ромка
    while UsbAsp25_Busy() do
    begin
      Application.ProcessMessages;
      if UserCancel then Exit;
    end;

    LogPrint(STR_NEW_SREG+IntToBin(sreg, 8));
  end;


finally
  ExitProgMode25;
  AsProgrammer.Programmer.DevClose;
  UnlockControl();
end;

end;


// Thêm hằng số SPI_CMD_25_NAND vào phần const nếu chưa có
// const
//   ...
//   SPI_CMD_25_NAND        = 4; // hoặc giá trị phù hợp với thứ tự trong ComboSPICMD

// Cập nhật hàm MenuItemReadSregClick
procedure TMainForm.MenuItemReadSregClick(Sender: TObject);
var
  sreg, sreg2, sreg3: byte;
  UsedSPICmd: byte; // Biến để theo dõi loại SPI đang sử dụng
begin
  UsedSPICmd := 255; // Khởi tạo giá trị mặc định không hợp lệ
  try
    ButtonCancel.Tag := 0;
    if not OpenDevice() then exit;
    sreg:= 0;
    LockControl();

    // Ghi lại loại chip đang sử dụng để dùng trong finally
    UsedSPICmd := ComboSPICMD.ItemIndex;

    // Vào chế độ lập trình dựa trên loại chip được chọn
    if ComboSPICMD.ItemIndex = SPI_CMD_KB then
      EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked) // KB có thể dùng logic 25?
    else if ComboSPICMD.ItemIndex = SPI_CMD_25_NAND then
      EnterProgMode25NAND(SetSPISpeed(0)) // Gọi hàm NAND
    else // SPI_CMD_25, SPI_CMD_45, SPI_CMD_95
      EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked);

    if ComboSPICMD.ItemIndex = SPI_CMD_25 then
    begin
      UsbAsp25_ReadSR(sreg); //Читаем регистр
      UsbAsp25_ReadSR(sreg2, $35); //Второй байт
      UsbAsp25_ReadSR(sreg3, $15); //Третий байт
      LogPrint('Sreg: '+IntToBin(sreg, 8)+'(0x'+(IntToHex(sreg, 2)+'), ')
                                           +IntToBin(sreg2, 8)+'(0x'+(IntToHex(sreg2, 2)+'), ')
                                           +IntToBin(sreg3, 8)+'(0x'+(IntToHex(sreg3, 2)+')'));
    end
    else if ComboSPICMD.ItemIndex = SPI_CMD_25_NAND then // Thêm nhánh cho NAND
    begin
      // SPI NAND sử dụng các thanh ghi Feature Register khác nhau
      // Đọc các thanh ghi chính: Status, Protection, Config
      // Giả sử các hằng số SPI_NAND_FEATURE_STATUS, SPI_NAND_FEATURE_PROTECTION, SPI_NAND_FEATURE_CONFIG
      // đã được định nghĩa trong spi25NAND.pas

      // Đọc Status Register (Feature Address $C0)
      if UsbAsp25NAND_ReadStatus(sreg, SPI_NAND_FEATURE_STATUS) < 0 then
      begin
        LogPrint('Error reading SPI NAND Status Register.');
        Exit; // hoặc tiếp tục đọc các thanh ghi khác nếu muốn
      end;

      // Đọc Protection Register (Feature Address $A0)
      if UsbAsp25NAND_ReadStatus(sreg2, SPI_NAND_FEATURE_PROTECTION) < 0 then
      begin
        LogPrint('Error reading SPI NAND Protection Register.');
        // Có thể tiếp tục hoặc Exit
        sreg2 := $FF; // Gán giá trị mặc định nếu lỗi
      end;

      // Đọc Config Register (Feature Address $B0) - có thể chứa ECC enable và các bit khác
      if UsbAsp25NAND_ReadStatus(sreg3, SPI_NAND_FEATURE_CONFIG) < 0 then
      begin
        LogPrint('Error reading SPI NAND Config Register.');
        // Có thể tiếp tục hoặc Exit
        sreg3 := $FF; // Gán giá trị mặc định nếu lỗi
      end;

      LogPrint('SPI NAND Sreg (Status $C0): '+IntToBin(sreg, 8)+'(0x'+(IntToHex(sreg, 2)+'), ')
                                           +'Protection $A0: '+IntToBin(sreg2, 8)+'(0x'+(IntToHex(sreg2, 2)+'), ')
                                           +'Config $B0: '+IntToBin(sreg3, 8)+'(0x'+(IntToHex(sreg3, 2)+')'));
    end
    else if ComboSPICMD.ItemIndex = SPI_CMD_95 then
    begin
      UsbAsp95_ReadSR(sreg); //Читаем регистр
      LogPrint('Sreg: '+IntToBin(sreg, 8));
    end
    else if ComboSPICMD.ItemIndex = SPI_CMD_45 then
    begin
      UsbAsp45_ReadSR(sreg); //Читаем регистр
      LogPrint('Sreg: '+IntToBin(sreg, 8));
    end;

  finally
    // Gọi hàm ExitProgMode phù hợp dựa trên loại chip đã sử dụng
    if UsedSPICmd = SPI_CMD_25_NAND then
      ExitProgMode25NAND // Gọi hàm NAND
    else if UsedSPICmd in [SPI_CMD_25, SPI_CMD_45, SPI_CMD_95, SPI_CMD_KB] then // Các loại khác dùng 25
      ExitProgMode25; // Gọi hàm NOR
    // I2C và MW có logic riêng, không cần kiểm tra UsedSPICmd

    AsProgrammer.Programmer.DevClose;
    UnlockControl();
  end;
end;

procedure TMainForm.RadioI2CChange(Sender: TObject);
begin
  Label1.Visible              := True;
  Label4.Visible              := True;
  ComboAddrType.Visible       := True;
  ComboPageSize.Visible       := True;
  Label5.Visible              := False;
  LabelSPICMD.Visible         := False;
  LabelSpare.Visible          := False;
  ComboPageSpareSize.Visible  := False;
  ButtonReadID.Enabled        := False;
  ButtonBlock.Enabled         := False;
  ButtonErase.Enabled         := True;
  ComboMWBitLen.Visible       := False;
  ComboSPICMD.Visible         := False;
  Panel_I2C_DevAddr.Visible   := True;

  Label_StartPage.Visible       := False;
  StartPageEdit.Visible         := False;
  Label6.Visible                := True;
  StartAddressEdit.Visible      := True;
  Label_StartAddress.Visible    := True;

  ComboMWBitLen.Text:= 'MW addr len';
  ComboAddrType.Text:= '';
  ComboPageSize.Text:= 'Page size';
  ComboChipSize.Text:= 'Chip size';
  ComboPageSparesize.Text := 'Spare size';
end;

procedure TMainForm.RadioMwChange(Sender: TObject);
begin
  Label1.Visible              := False;
  ComboPageSize.Visible       := False;
  ComboAddrType.Visible       := False;
  ComboSPICMD.Visible         := False;
  ButtonReadID.Enabled        := False;
  ButtonBlock.Enabled         := False;
  Label4.Visible              := False;
  LabelSPICMD.Visible         := False;
  Panel_I2C_DevAddr.Visible   := False;
  Label5.Visible              := True;
  ButtonErase.Enabled         := True;
  ComboMWBitLen.Visible       := True;

  Label_StartPage.Visible       := False;
  StartPageEdit.Visible         := False;
  Label6.Visible                := True;
  StartAddressEdit.Visible      := True;
  Label_StartAddress.Visible    := True;


  ComboMWBitLen.Text:= 'MW addr len';
  ComboAddrType.Text:= '';
  ComboPageSize.Text:= 'Page size';
  ComboChipSize.Text:= 'Chip size';
  ComboPageSparesize.Text := 'Spare size';
end;

procedure TMainForm.RadioSPIChange(Sender: TObject);
var
  SkipFFLabel: string;
begin
  Label1.Visible              := True;
  LabelSPICMD.Visible         := True;
  ComboPageSize.Visible       := True;
  ComboSPICMD.Visible         := True;
  ButtonErase.Enabled         := True;
  ButtonReadID.Enabled        := True;

  if ComboSPICMD.ItemIndex = SPI_CMD_KB then
  begin
    ButtonBlock.Enabled := False;

    SkipFFLabel := MenuSkipFF.Caption;
    Delete(SkipFFLabel, Length(SkipFFLabel)-1 ,2);
    MenuSkipFF.Caption := SkipFFLabel + '00';
  end
  else
  begin
    ButtonBlock.Enabled := True;

    SkipFFLabel := MenuSkipFF.Caption;
    Delete(SkipFFLabel, Length(SkipFFLabel)-1 ,2);
    MenuSkipFF.Caption := SkipFFLabel + 'FF'
  end;

  if ComboSPICMD.ItemIndex = SPI_CMD_25_NAND then
  begin
    Label6.Visible                := False;
    StartAddressEdit.Visible      := False;
    Label_StartAddress.Visible    := False;
    Label_StartPage.Visible       := True;
    StartPageEdit.Visible         := True;
    LabelSpare.Visible          := True;
    ComboPageSpareSize.Visible  := True;
  end
  else
  begin
    Label_StartPage.Visible       := False;
    StartPageEdit.Visible         := False;
    LabelSpare.Visible          := False;
    ComboPageSpareSize.Visible  := False;
    Label6.Visible                := True;
    StartAddressEdit.Visible      := True;
    Label_StartAddress.Visible    := True;
  end;

  ComboMWBitLen.Visible       := False;
  Label4.Visible              := False;
  Label5.Visible              := False;
  ComboAddrType.Visible       := False;

  Panel_I2C_DevAddr.Visible  := False;

  ComboMWBitLen.Text:= 'MW addr len';
  ComboAddrType.Text:= '';
  ComboPageSize.Text:= 'Page size';
  ComboChipSize.Text:= 'Chip size';
  ComboPageSparesize.Text := 'Spare size';
end;



// Thêm hằng số SPI_CMD_25_NAND vào phần const nếu chưa có
// const
//   ...
//   SPI_CMD_25_NAND        = 4; // hoặc giá trị phù hợp với thứ tự trong ComboSPICMD

// Cập nhật hàm ButtonWriteClick
procedure TMainForm.ButtonWriteClick(Sender: TObject);
var
  PageSize: word;
  WriteType: byte;
  I2C_DevAddr: byte;
  I2C_ChunkSize: Word = 65535;
  UsedSPICmd: byte;
  TotalDataSize: cardinal;
begin
  UsedSPICmd := 255;
  try
    ButtonCancel.Tag := 0;
    if not OpenDevice() then exit;
    if Sender <> ComboItem1 then
      if MessageDlg('AsProgrammer', STR_START_WRITE, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;
    LockControl();

    if RunScriptFromFile(CurrentICParam.Script, 'write') then Exit;

    LogPrint(TimeToStr(Time()));

    if (not IsNumber(ComboChipSize.Text)) then
    begin
      LogPrint(STR_CHECK_SETTINGS);
      Exit;
    end;

    if MPHexEditorEx.DataSize > StrToInt(ComboChipSize.Text) - Hex2Dec('$'+StartAddressEdit.Text) then
    begin
      LogPrint(STR_WRONG_FILE_SIZE);
      Exit;
    end;

           // ========================================================================
    // SPI NAND - CHUNKED Write
    // ========================================================================
    if RadioSPI.Checked and (ComboSPICMD.ItemIndex = SPI_CMD_25_NAND) then
    begin
      UsedSPICmd := SPI_CMD_25_NAND;

      if not EnterProgMode25NAND(SetSPISpeed(0)) then
      begin
        LogPrint('Failed to enter programming mode');
        Exit;
      end; 
      
      TotalDataSize := MPHexEditorEx.DataSize;
      
      WriteFlash25NAND(StrToInt(StartPageEdit.Text), TotalDataSize, StrToInt(ComboPageSize.Text));
      
      // ======================================================================
      // AUTO VERIFY SAU KHI WRITE
      // ======================================================================
      if (MenuAutoCheck.Checked) then
        LogPrint(STR_TIME + TimeToStr(Time() - TimeCounter));
        TimeCounter := Time();
        begin
          // ================================================================
          // NAND: Dùng hàm wrapper đơn giản (tự động xử lý chunking)
          // ================================================================
          LogPrint('Starting auto-verify for NAND...');

          if not VerifyFlash25NAND_Chunked(StrToInt(StartPageEdit.Text),
                                           MPHexEditorEx.DataSize,
                                           True) then
          begin
            LogPrint('Auto-verify FAILED!');
          end;
        end 
    end

    // SPI
    else if RadioSPI.Checked then
    begin
      UsedSPICmd := ComboSPICMD.ItemIndex;

      if ComboSPICMD.ItemIndex = SPI_CMD_KB then
        EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked)
      else
        EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked);

      if ComboSPICMD.ItemIndex <> SPI_CMD_KB then
        IsLockBitsEnabled;

      if (not IsNumber(ComboPageSize.Text)) and (UpperCase(ComboPageSize.Text)<>'SSTB') and (UpperCase(ComboPageSize.Text)<>'SSTW') then
      begin
        LogPrint(STR_CHECK_SETTINGS);
        Exit;
      end;

      TimeCounter := Time();

      RomF.Clear;
      RomF.Position := 0;
      MPHexEditorEx.SaveToStream(RomF);
      RomF.Position := 0;

      // Parse page size và write type
      if UpperCase(ComboPageSize.Text)='SSTB' then
      begin
        PageSize := 1;
        WriteType := WT_SSTB;
      end
      else if UpperCase(ComboPageSize.Text)='SSTW' then
      begin
        PageSize := 2;
        WriteType := WT_SSTW;
      end
      else if IsNumber(ComboPageSize.Text) then
      begin
        PageSize := StrToInt(ComboPageSize.Text);
        if PageSize < 1 then
        begin
          PageSize := 1;
          ComboPageSize.Text := '1';
        end;
        WriteType := WT_PAGE;
      end
      else
      begin
        LogPrint(STR_CHECK_SETTINGS);
        Exit;
      end;

      // Write dữ liệu
      if ComboSPICMD.ItemIndex = SPI_CMD_25 then
        WriteFlash25(RomF, Hex2Dec('$'+StartAddressEdit.Text), MPHexEditorEx.DataSize, PageSize, WriteType)
      else if ComboSPICMD.ItemIndex = SPI_CMD_95 then
        WriteFlash95(RomF, Hex2Dec('$'+StartAddressEdit.Text), MPHexEditorEx.DataSize, PageSize, StrToInt(ComboChipSize.Text))
      else if ComboSPICMD.ItemIndex = SPI_CMD_45 then
        WriteFlash45(RomF, 0, MPHexEditorEx.DataSize, PageSize, WriteType)
      else if ComboSPICMD.ItemIndex = SPI_CMD_KB then
        WriteFlashKB(RomF, 0, MPHexEditorEx.DataSize, PageSize);
      

      // ======================================================================
      // AUTO VERIFY SAU KHI WRITE
      // ======================================================================
      if (MenuAutoCheck.Checked) and (WriteType <> WT_SSTB) and (WriteType <> WT_SSTW) then
      begin
        LogPrint(STR_TIME + TimeToStr(Time() - TimeCounter));
        TimeCounter := Time();


        if ComboSPICMD.ItemIndex = SPI_CMD_KB then
          begin
            // KB
            RomF.Position := 0;
            MPHexEditorEx.SaveToStream(RomF);
            RomF.Position := 0;
            VerifyFlashKB(RomF, 0, MPHexEditorEx.DataSize);
          end
        else
        begin
          // NOR (25, 95, 45)
          RomF.Position := 0;
          MPHexEditorEx.SaveToStream(RomF);
          RomF.Position := 0;
          VerifyFlash25(RomF, Hex2Dec('$'+StartAddressEdit.Text), MPHexEditorEx.DataSize);
        end;
      end;
    end;

    // I2C và MW (giữ nguyên)
    //I2C
    if RadioI2C.Checked then
    begin
      if ( (ComboAddrType.ItemIndex < 0) or (not IsNumber(ComboPageSize.Text)) ) then
      begin
        LogPrint(STR_CHECK_SETTINGS);
        Exit;
      end;

      EnterProgModeI2C();

      //Адрес микросхемы по чекбоксам
      I2C_DevAddr := SetI2CDevAddr();

      if CheckBox_I2C_ByteRead.Checked then I2C_ChunkSize := 1;

      if UsbAspI2C_BUSY(I2C_DevAddr) then
      begin
        LogPrint(STR_I2C_NO_ANSWER);
        exit;
      end;
      TimeCounter := Time();

      RomF.Position := 0;
      MPHexEditorEx.SaveToStream(RomF);
      RomF.Position := 0;

      if StrToInt(ComboPageSize.Text) < 1 then ComboPageSize.Text := '1';

      WriteFlashI2C(RomF, Hex2Dec('$'+StartAddressEdit.Text), MPHexEditorEx.DataSize, StrToInt(ComboPageSize.Text), I2C_DevAddr);

      if MenuAutoCheck.Checked then
      begin
        if UsbAspI2C_BUSY(I2C_DevAddr) then
        begin
          LogPrint(STR_I2C_NO_ANSWER);
          exit;
        end;
        LogPrint(STR_TIME + TimeToStr(Time() - TimeCounter));

        TimeCounter := Time();

        RomF.Position :=0;
        MPHexEditorEx.SaveToStream(RomF);
        RomF.Position :=0;
        VerifyFlashI2C(RomF, Hex2Dec('$'+StartAddressEdit.Text), RomF.Size, I2C_ChunkSize, I2C_DevAddr);
      end;

    end;
    //Microwire
    if RadioMW.Checked then
    begin
      if (not IsNumber(ComboMWBitLen.Text)) then
      begin
        LogPrint(STR_CHECK_SETTINGS);
        Exit;
      end;

      AsProgrammer.Programmer.MWInit(SetSPISpeed(0));
      TimeCounter := Time();

      RomF.Position := 0;
      MPHexEditorEx.SaveToStream(RomF);
      RomF.Position := 0;

      WriteFlashMW(RomF, StrToInt(ComboMWBitLen.Text), 0, MPHexEditorEx.DataSize);

      if MenuAutoCheck.Checked then
      begin
        TimeCounter := Time();
        RomF.Position :=0;
        MPHexEditorEx.SaveToStream(RomF);
        RomF.Position :=0;
        VerifyFlashMW(RomF, StrToInt(ComboMWBitLen.Text), 0, StrToInt(ComboChipSize.Text));
      end;

    end;

    LogPrint(STR_TIME + TimeToStr(Time() - TimeCounter));

  finally
    if UsedSPICmd = SPI_CMD_25_NAND then
      ExitProgMode25NAND
    else if UsedSPICmd in [SPI_CMD_25, SPI_CMD_45, SPI_CMD_95, SPI_CMD_KB] then
      ExitProgMode25;

    AsProgrammer.Programmer.DevClose;
    UnlockControl();
  end;
end;

procedure TMainForm.ButtonVerifyClick(Sender: TObject);
begin
  VerifyFlash(false);
end;


// Thêm hằng số SPI_CMD_25_NAND vào phần const nếu chưa có
// const
//   ...
//   SPI_CMD_25_NAND        = 4; // hoặc giá trị phù hợp với thứ tự trong ComboSPICMD

// Cập nhật hàm VerifyFlash
procedure TMainForm.VerifyFlash(BlankCheck: boolean = false);
var
  I2C_DevAddr: byte;
  I2C_ChunkSize: Word = 65535;
  i: Longword;
  BlankByte: byte;
  UsedSPICmd: byte;
  StartPageValue: cardinal;
  TotalDataSize: cardinal;
begin
  UsedSPICmd := 255;
  try
    ButtonCancel.Tag := 0;
    if not OpenDevice() then exit;
    LockControl();

    if RunScriptFromFile(CurrentICParam.Script, 'verify') then Exit;

    LogPrint(TimeToStr(Time()));

    if not IsNumber(ComboChipSize.Text) then
    begin
      LogPrint(STR_CHECK_SETTINGS);
      Exit;
    end;

    // ========================================================================
    // SPI NAND - CHUNKED VERIFY
    // ========================================================================
    if RadioSPI.Checked and (ComboSPICMD.ItemIndex = SPI_CMD_25_NAND) then
    begin
      UsedSPICmd := SPI_CMD_25_NAND;

      if not EnterProgMode25NAND(SetSPISpeed(0)) then
      begin
        LogPrint('Failed to enter programming mode');
        Exit;
      end;

      TimeCounter := Time();

      // Lấy StartPage
      if IsNumber(StartPageEdit.Text) then
        StartPageValue := StrToInt(StartPageEdit.Text)
      else
        StartPageValue := 0;

      // Xử lý Blank Check hoặc File
      if BlankCheck then
        begin
          TotalDataSize := StrToInt(ComboChipSize.Text);
          LogPrint('Blank check mode: Verifying ' + IntToStr(TotalDataSize) + ' bytes as $FF');
          VerifyFlash25NAND_Chunked(StartPageValue, TotalDataSize, True, True); // ← Thêm tham số BlankCheck = True
        end
        else
        begin
          TotalDataSize := MPHexEditorEx.DataSize;
          VerifyFlash25NAND_Chunked(StartPageValue, TotalDataSize, True, False); // ← BlankCheck = False
        end;
    end

    // ========================================================================
    // CÁC LOẠI CHIP KHÁC - GIỮ NGUYÊN
    // ========================================================================
    else if RadioSPI.Checked then
    begin
      UsedSPICmd := ComboSPICMD.ItemIndex;

      if ComboSPICMD.ItemIndex = SPI_CMD_KB then
        EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked)
      else
        EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked);

      TimeCounter := Time();

      if RomF = nil then
      begin
        LogPrint('Error: RomF is nil!');
        Exit;
      end;

      RomF.Clear;
      if BlankCheck then
      begin
        if ComboSPICMD.ItemIndex = SPI_CMD_KB then
          BlankByte := $00
        else
          BlankByte := $FF;

        for i:=1 to StrToInt(ComboChipSize.Text) do
          RomF.WriteByte(BlankByte);
      end
      else
      begin
        if MPHexEditorEx.DataSize = 0 then
        begin
          LogPrint('Warning: MPHexEditorEx has no data!');
          Exit;
        end;

        try
          MPHexEditorEx.SaveToStream(RomF);
        except
          on E: Exception do
          begin
            LogPrint('ERROR in SaveToStream: ' + E.ClassName + ' - ' + E.Message);
            raise;
          end;
        end;
      end;

      RomF.Position := 0;

      if ComboSPICMD.ItemIndex = SPI_CMD_KB then
        VerifyFlashKB(RomF, 0, RomF.Size)
      else if ComboSPICMD.ItemIndex = SPI_CMD_25 then
        VerifyFlash25(RomF, Hex2Dec('$'+StartAddressEdit.Text), RomF.Size)
      else if ComboSPICMD.ItemIndex = SPI_CMD_95 then
        VerifyFlash95(RomF, Hex2Dec('$'+StartAddressEdit.Text), RomF.Size, StrToInt(ComboChipSize.Text))
      else if ComboSPICMD.ItemIndex = SPI_CMD_45 then
      begin
        if (not IsNumber(ComboPageSize.Text)) then
        begin
          LogPrint(STR_CHECK_SETTINGS);
          Exit;
        end;
        VerifyFlash45(RomF, 0, StrToInt(ComboPageSize.Text), RomF.Size);
      end;
    end;

    // I2C và MW (giữ nguyên code của bạn)
    //I2C
    if RadioI2C.Checked then
    begin
      if ComboAddrType.ItemIndex < 0 then
      begin
        LogPrint(STR_CHECK_SETTINGS);
        Exit;
      end;

      EnterProgModeI2C();

      //Адрес микросхемы по чекбоксам
      I2C_DevAddr := SetI2CDevAddr();

      if CheckBox_I2C_ByteRead.Checked then I2C_ChunkSize := 1;

      if UsbAspI2C_BUSY(I2C_DevAddr) then
      begin
        LogPrint(STR_I2C_NO_ANSWER);
        exit;
      end;
      TimeCounter := Time();

      RomF.Clear;
      if BlankCheck then
      begin
        for i:=1 to StrToInt(ComboChipSize.Text) do
          RomF.WriteByte($FF);
      end
      else
        MPHexEditorEx.SaveToStream(RomF);
      RomF.Position :=0;

      VerifyFlashI2C(RomF, Hex2Dec('$'+StartAddressEdit.Text), RomF.Size, I2C_ChunkSize, I2C_DevAddr);
    end;

    //Microwire
    if RadioMW.Checked then
    begin
      if (not IsNumber(ComboMWBitLen.Text)) then
      begin
        LogPrint(STR_CHECK_SETTINGS);
        Exit;
      end;

      AsProgrammer.Programmer.MWInit(SetSPISpeed(0));
      TimeCounter := Time();

      RomF.Clear;
      if BlankCheck then
      begin
        for i:=1 to StrToInt(ComboChipSize.Text) do
          RomF.WriteByte($FF);
      end
      else
        MPHexEditorEx.SaveToStream(RomF);
      RomF.Position :=0;

      VerifyFlashMW(RomF, StrToInt(ComboMWBitLen.Text), 0, RomF.Size);
    end;

    LogPrint(STR_TIME + TimeToStr(Time() - TimeCounter));

  finally
    if UsedSPICmd = SPI_CMD_25_NAND then
      ExitProgMode25NAND
    else if UsedSPICmd in [SPI_CMD_25, SPI_CMD_45, SPI_CMD_95, SPI_CMD_KB] then
      ExitProgMode25;

    AsProgrammer.Programmer.DevClose;
    UnlockControl();
  end;
end;

// Cập nhật hàm ButtonBlockClick
procedure TMainForm.ButtonBlockClick(Sender: TObject);
var
  sreg: byte;
  i: integer;
  s: string;
  SLreg: array[0..31] of byte;
  UsedSPICmd: byte; // Biến để theo dõi loại SPI đang sử dụng
begin
  UsedSPICmd := 255; // Khởi tạo giá trị mặc định không hợp lệ
  try
    ButtonCancel.Tag := 0;
    if not OpenDevice() then exit;
    sreg := 0;
    LockControl();

    if RunScriptFromFile(CurrentICParam.Script, 'unlock') then Exit;

    // Ghi lại loại chip đang sử dụng để dùng trong finally
    UsedSPICmd := ComboSPICMD.ItemIndex;

    // Vào chế độ lập trình dựa trên loại chip được chọn
    if ComboSPICMD.ItemIndex = SPI_CMD_KB then
      EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked) // KB có thể dùng logic 25?
    else if ComboSPICMD.ItemIndex = SPI_CMD_25_NAND then
      EnterProgMode25NAND(SetSPISpeed(0)) // Gọi hàm NAND
    else // SPI_CMD_25, SPI_CMD_45, SPI_CMD_95
      EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked);

    if ComboSPICMD.ItemIndex = SPI_CMD_25 then
    begin
      UsbAsp25_ReadSR(sreg); //Читаем регистр
      LogPrint(STR_OLD_SREG+IntToBin(sreg, 8)+'(0x'+(IntToHex(sreg, 2)+')'));

      sreg := 0;

      UsbAsp25_WREN(); //Включаем разрешение записи
      UsbAsp25_WriteSR(sreg); //Сбрасываем регистр

      //Пока отлипнет ромка
      while UsbAsp25_Busy() do
      begin
        Application.ProcessMessages;
        if UserCancel then Exit;
      end;

      UsbAsp25_ReadSR(sreg); //Читаем регистр
      LogPrint(STR_NEW_SREG+IntToBin(sreg, 8)+'(0x'+(IntToHex(sreg, 2)+')'));
    end
    else if ComboSPICMD.ItemIndex = SPI_CMD_25_NAND then // Thêm nhánh cho NAND
    begin
        // Gọi hàm gỡ bảo vệ từ spi25NAND.pas
        if UsbAsp25NAND_Unprotect() < 0 then
        begin
            LogPrint('Failed to unprotect SPI NAND chip.');
        end;
        // LogPrint đã được thực hiện bên trong UsbAsp25NAND_Unprotect
    end
    else if ComboSPICMD.ItemIndex = SPI_CMD_95 then
    begin
      UsbAsp95_ReadSR(sreg); //Читаем регистр
      LogPrint(STR_OLD_SREG+IntToBin(sreg, 8));

      sreg := 0; //
      UsbAsp95_WREN(); //Включаем разрешение записи
      UsbAsp95_WriteSR(sreg); //Сбрасываем регистр

      //Пока отлипнет ромка
      while UsbAsp25_Busy() do
      begin
        Application.ProcessMessages;
        if UserCancel then Exit;
      end;

      UsbAsp95_ReadSR(sreg); //Читаем регистр
      LogPrint(STR_NEW_SREG+IntToBin(sreg, 8));
    end
    else if ComboSPICMD.ItemIndex = SPI_CMD_45 then
    begin
      UsbAsp45_DisableSP();
      UsbAsp45_ReadSR(sreg); //Читаем регистр
      LogPrint('Sreg: '+IntToBin(sreg, 8));

      UsbAsp45_ReadSectorLockdown(SLreg); //Читаем Lockdown регистр

      s := '';
      for i:=0 to 31 do
      begin
        s := s + IntToHex(SLreg[i], 2);
      end;
      LogPrint('Secktor Lockdown регистр: 0x'+s);
      if UsbAsp45_isPagePowerOfTwo() then LogPrint(STR_45PAGE_POWEROF2)
        else LogPrint(STR_45PAGE_STD);

    end;


  finally
    // Gọi hàm ExitProgMode phù hợp dựa trên loại chip đã sử dụng
    if UsedSPICmd = SPI_CMD_25_NAND then
      ExitProgMode25NAND // Gọi hàm NAND
    else if UsedSPICmd in [SPI_CMD_25, SPI_CMD_45, SPI_CMD_95, SPI_CMD_KB] then // Các loại khác dùng 25
      ExitProgMode25; // Gọi hàm NOR
    // I2C và MW có logic riêng, không cần kiểm tra UsedSPICmd

    AsProgrammer.Programmer.DevClose;
    UnlockControl();
  end;

end;


// main.pas - ButtonReadIDClick (Simplified)
// ============================================================================
procedure TMainForm.ButtonReadIDClick(Sender: TObject);
var
  XMLfile: TXMLDocument;
  ID: MEMORY_ID;
  ID_NAND: MEMORY_ID_NAND;
  IDstr9FH, IDstr90H, IDstrABH, IDstr15H: string;
  
  // SPI NAND specific
  ParsedID: TParsedChipID;
  SearchSuccess: boolean;
begin
  try
    if not OpenDevice() then exit;
    LockControl();

    // Khởi tạo
    FillByte(ID.ID9FH, 3, $FF);
    FillByte(ID.ID90H, 2, $FF);
    FillByte(ID.IDABH, 1, $FF);
    FillByte(ID.ID15H, 2, $FF);
    FillByte(ID_NAND.ID9FH, 3, $FF);

    // ========================================================================
    // ENTER PROGRAM MODE & READ ID
    // ========================================================================
    if ComboSPICMD.ItemIndex = SPI_CMD_KB then
    begin
      // KB9012 - giữ nguyên
      EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked);
      UsbAspMulti_EnableEDI();
      UsbAspMulti_EnableEDI();
      UsbAspMulti_ReadReg($FF00, ID.IDABH);
      LogPrint('KB9012 EC Hardware version: ' + IntToHex(ID.IDABH, 2));
      UsbAspMulti_ReadReg($FF24, ID.IDABH);
      LogPrint('KB9012 EDI version: ' + IntToHex(ID.IDABH, 2));
      ExitProgMode25;
      Exit;
    end
    else if ComboSPICMD.ItemIndex = SPI_CMD_25_NAND then
    begin
      // ======================================================================
      // SPI NAND - Sử dụng hàm từ spi25nand.pas
      // ======================================================================
      EnterProgMode25NAND(SetSPISpeed(0));
      UsbAsp25NAND_ReadID(ID_NAND);
      ExitProgMode25NAND;
      
      // Parse ID bằng hàm trong spi25nand.pas
      ParsedID := ParseChipID(ID_NAND);
      
      // Kiểm tra ID có hợp lệ không
      if not ParsedID.IsValid then
      begin
        LogPrint('Error: Invalid chip ID (all 0x00 or 0xFF)');
        Exit;
      end;
      
      // In thông tin chi tiết
      LogPrint('========================================');
      LogPrint('SPI NAND Flash Detected');
      LogPrint('========================================');
      LogPrint(FormatChipIDString(ParsedID));
      LogPrint('========================================');
    end
    else
    begin
      // ======================================================================
      // SPI NOR - giữ nguyên logic cũ
      // ======================================================================
      EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked);
      UsbAsp25_ReadID(ID);
      ExitProgMode25;
      
      IDstr9FH := UpperCase(IntToHex(ID.ID9FH[0], 2) + 
                           IntToHex(ID.ID9FH[1], 2) + 
                           IntToHex(ID.ID9FH[2], 2));
      IDstr90H := UpperCase(IntToHex(ID.ID90H[0], 2) + IntToHex(ID.ID90H[1], 2));
      IDstrABH := UpperCase(IntToHex(ID.IDABH, 2));
      IDstr15H := UpperCase(IntToHex(ID.ID15H[0], 2) + IntToHex(ID.ID15H[1], 2));

      LogPrint('ID(9F): ' + IDstr9FH + STR_ID_UNKNOWN);
      LogPrint('ID(90): ' + IDstr90H + STR_ID_UNKNOWN);
      LogPrint('ID(AB): ' + IDstrABH + STR_ID_UNKNOWN);
      LogPrint('ID(15): ' + IDstr15H + STR_ID_UNKNOWN);
    end;

    AsProgrammer.Programmer.DevClose;

    // ========================================================================
    // SEARCH CHIP IN chiplist.xml
    // ========================================================================
    if (ComboSPICMD.ItemIndex = SPI_CMD_25) or 
       (ComboSPICMD.ItemIndex = SPI_CMD_25_NAND) then
    begin
      if not FileExists('chiplist.xml') then
      begin
        LogPrint('Error: chiplist.xml not found!');
        Exit;
      end;

      try
        ReadXMLFile(XMLfile, 'chiplist.xml');
      except
        on E: EXMLReadError do
        begin
          ShowMessage('Error reading chiplist.xml: ' + E.Message);
          Exit;
        end;
      end;

      try
        ChipSearchForm.ListBoxChips.Clear;
        ChipSearchForm.EditSearch.Text := '';
        SearchSuccess := False;

        if ComboSPICMD.ItemIndex = SPI_CMD_25 then
        begin
          // ====================================================================
          // SPI NOR: Tìm theo thứ tự ưu tiên
          // ====================================================================
          FindChip.FindChip(XMLfile, '', IDstr9FH);
          if ChipSearchForm.ListBoxChips.Items.Capacity = 0 then 
            FindChip.FindChip(XMLfile, '', IDstr90H);
          if ChipSearchForm.ListBoxChips.Items.Capacity = 0 then 
            FindChip.FindChip(XMLfile, '', IDstrABH);
          if ChipSearchForm.ListBoxChips.Items.Capacity = 0 then 
            FindChip.FindChip(XMLfile, '', IDstr15H);
            
          SearchSuccess := ChipSearchForm.ListBoxChips.Items.Capacity > 0;
        end
        else if ComboSPICMD.ItemIndex = SPI_CMD_25_NAND then
        begin
          // ====================================================================
          // SPI NAND: Tìm với ID đã parse (đơn giản!)
          // ====================================================================
          LogPrint('Searching in chiplist.xml for ID: ' + ParsedID.IDString);
          
          FindChip.FindChip(XMLfile, '', ParsedID.IDString);
          SearchSuccess := ChipSearchForm.ListBoxChips.Items.Capacity > 0;
          
          if SearchSuccess then
          begin
            LogPrint('✓ Found ' + IntToStr(ChipSearchForm.ListBoxChips.Items.Count) + 
                    ' matching chip(s)');
          end
          else
          begin
            LogPrint('✗ Chip not found in database');
            LogPrint('');
            LogPrint('To add this chip to chiplist.xml:');
            LogPrint('1. Get chip specifications from datasheet');
            LogPrint('2. Add entry under <SPI_NAND>/<' + 
                    UpperCase(ParsedID.MfgInfo.Name) + '>:');
            LogPrint('   <YourChipName id="' + ParsedID.IDString + 
                    '" page="2048" size="..." spare="..." spicmd="25N" planes="..." block="64"/>');
            LogPrint('');
            
            // Gợi ý các chip tương tự (cùng manufacturer)
            if ParsedID.MfgInfo.Name <> 'Unknown' then
            begin
              LogPrint('Similar chips from ' + ParsedID.MfgInfo.Name + ':');
              FindChip.FindChip(XMLfile, ParsedID.MfgInfo.Name, '');
              if ChipSearchForm.ListBoxChips.Items.Capacity > 0 then
                SearchSuccess := True;  // Show form với gợi ý
            end;
          end;
        end;

        // Show results
        if SearchSuccess then
          ChipSearchForm.Show
        else if ComboSPICMD.ItemIndex = SPI_CMD_25_NAND then
          LogPrint('No chips found. Please update chiplist.xml');

      finally
        XMLfile.Free;
      end;
    end;

  finally
    UnlockControl();
  end;
end;


procedure TMainForm.ButtonOpenHexClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
   MPHexEditorEx.LoadFromFile(OpenDialog.FileName);
   StatusBar.Panels.Items[2].Text := OpenDialog.FileName;
  end;
end;

procedure TMainForm.ButtonSaveHexClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    MPHexEditorEx.SaveToFile(SaveDialog.FileName);
    StatusBar.Panels.Items[2].Text := SaveDialog.FileName;
  end;
end;

procedure TMainForm.ButtonCancelClick(Sender: TObject);
begin
  ButtonCancel.Tag:= 1;
  ScriptEngine.Stop:= true;
end;

procedure TMainForm.I2C_DevAddrChange(Sender: TObject);
begin
  if TToggleBox(Sender).State = cbUnchecked then
  TToggleBox(Sender).Caption:= '0';
  if TToggleBox(Sender).State = cbChecked then
  TToggleBox(Sender).Caption:= '1';
end;

procedure TMainForm.ScriptsMenuItemClick(Sender: TObject);
begin
  ScriptEditForm.Show;
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  if ComboBox_chip_scriptrun.Items.Capacity < 1 then Exit;;
  if not OpenDevice() then exit;
  if RunScriptFromFile(CurrentICParam.Script, ComboBox_chip_scriptrun.Text) then Exit;
end;

procedure TMainForm.StartAddressEditChange(Sender: TObject);
begin
  if StartAddressEdit.Text = '' then StartAddressEdit.Text := '0';
  if Hex2Dec('$'+StartAddressEdit.Text) > 0 then
     StartAddressEdit.Color:= clYellow
  else
     StartAddressEdit.Color:= clDefault;
end;



procedure TMainForm.StartAddressEditKeyPress(Sender: TObject; var Key: char);
begin
  Key := UpCase(Key);
  if not(Key in['A'..'F', '0'..'9', Char(VK_BACK)]) then Key := Char('');
end;

procedure TMainForm.StartPageEditChange(Sender: TObject);
var
  val: Integer;
begin
  if StartPageEdit.Text = '' then
    StartPageEdit.Text := '0';

  val := StrToIntDef(StartPageEdit.Text, 0);

  if val > 0 then
    StartPageEdit.Color := clYellow
  else
    StartPageEdit.Color := clDefault;
end;

 procedure TMainForm.StartPageEditKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', #8]) then  // #8 = Backspace
    Key := #0;
end;

procedure LoadChipList(XMLfile: TXMLDocument);
var
  Node: TDOMNode;
  j, i: integer;
begin
  if XMLfile <> nil then
  begin

    Node := XMLfile.DocumentElement.FirstChild;

    while Assigned(Node) do
    begin

     if (LowerCase(Node.NodeName) = 'options') or (LowerCase(Node.NodeName) = 'locale') then
     begin
       Node := Node.NextSibling;
       continue;
     end;

     MainForm.MenuChip.Add(NewItem(UTF16ToUTF8(Node.NodeName), 0, False, True, nil, 0, '')); //Раздел(SPI, I2C...)

     // Используем свойство ChildNodes
     with Node.ChildNodes do
     try
       for j := 0 to (Count - 1) do
       begin
         MainForm.MenuChip.Find(UTF16ToUTF8(Node.NodeName)).Add(NewItem(UTF16ToUTF8(Item[j].NodeName) ,0, False, True, nil, 0, '')); //Раздел Фирма

         for i := 0 to (Item[j].ChildNodes.Count - 1) do
           MainForm.MenuChip.Find(UTF16ToUTF8(Node.NodeName)).
             Find(UTF16ToUTF8(Item[j].NodeName)).
               Add(NewItem(UTF16ToUTF8(Item[j].ChildNodes.Item[i].NodeName), 0, False, True, @MainForm.ChipClick, 0, '' )); //Чип
       end;
     finally
       Free;
     end;
     Node := Node.NextSibling;
    end;
  end;

end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AsProgrammer := TAsProgrammer.Create;
  AsProgrammer.AddHW(TUsbAspHardware.Create);
  AsProgrammer.AddHW(TCH341Hardware.Create);
  AsProgrammer.AddHW(TAvrispHardware.Create);
  AsProgrammer.AddHW(TArduinoHardware.Create);
  AsProgrammer.AddHW(TFT232HHardware.Create);
  AsProgrammer.AddHW(TCH347Hardware.Create);

  LoadChipList(ChipListFile);
  RomF := TMemoryStream.Create;
  ScriptEngine := TPasCalc.Create;
  ScriptsFunc.SetScriptFunctions(ScriptEngine);

  MPHexEditorEx.NoSizeChange := true;
  MPHexEditorEx.InsertMode := false;
  LoadOptions(SettingsFile);
  LoadLangList();
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  AsProgrammer.Free;
  MainForm.MPHexEditorEx.Free;
  RomF.Free;
  SaveOptions(SettingsFile);
  ChipListFile.Free;
  SettingsFile.Free;
  ScriptEngine.Free;
end;


procedure TMainForm.ButtonReadClick(Sender: TObject);
var
  I2C_DevAddr: byte;
  I2C_ChunkSize: word = 65535;
  CRC32: Cardinal;
  // Biến cục bộ để theo dõi loại chip trong hàm này
  UsedSPICmd: byte;
begin
  UsedSPICmd := 255; // Khởi tạo giá trị mặc định không hợp lệ
  try
    ButtonCancel.Tag := 0;
    if not OpenDevice() then exit;
    LockControl();

    if RunScriptFromFile(CurrentICParam.Script, 'read') then Exit;

    LogPrint(TimeToStr(Time()));

    if (not IsNumber(ComboChipSize.Text)) then
    begin
      LogPrint(STR_CHECK_SETTINGS);
      Exit;
    end;

    //SPI
    if RadioSPI.Checked then
    begin
      // Ghi lại loại chip đang sử dụng để dùng trong finally
      UsedSPICmd := ComboSPICMD.ItemIndex;

      // Vào chế độ lập trình dựa trên loại chip được chọn
      if ComboSPICMD.ItemIndex = SPI_CMD_KB then
        EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked) // KB có thể dùng logic 25?
      else if ComboSPICMD.ItemIndex = SPI_CMD_25_NAND then
        EnterProgMode25NAND(SetSPISpeed(0)) // Gọi hàm NAND
      else // SPI_CMD_25, SPI_CMD_45, SPI_CMD_95
        EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked);

      TimeCounter := Time();

      if  ComboSPICMD.ItemIndex = SPI_CMD_KB then
      begin
        ReadFlashKB(RomF, 0, StrToInt(ComboChipSize.Text));
      end
      else if  ComboSPICMD.ItemIndex = SPI_CMD_25 then
        ReadFlash25(RomF, Hex2Dec('$'+StartAddressEdit.Text), StrToInt(ComboChipSize.Text))
      else if  ComboSPICMD.ItemIndex = SPI_CMD_45 then
      begin
        if (not IsNumber(ComboPageSize.Text)) then
        begin
          LogPrint(STR_CHECK_SETTINGS);
          Exit;
        end;
        ReadFlash45(RomF, 0, StrToInt(ComboPageSize.Text), StrToInt(ComboChipSize.Text));
      end
      else if  ComboSPICMD.ItemIndex = SPI_CMD_95 then
        ReadFlash95(RomF, Hex2Dec('$'+StartAddressEdit.Text), StrToInt(ComboChipSize.Text))
      else if  ComboSPICMD.ItemIndex = SPI_CMD_25_NAND then // Thêm nhánh cho NAND
        //ReadFlash25NAND(RomF, StrToInt(StartPageEdit.Text), StrToInt(ComboChipSize.Text)); // Gọi hàm NAND
        ReadFlash25NAND(RomF, StrToInt(StartPageEdit.Text), StrToInt(ComboChipSize.Text));  // Gọi hàm NAND

      RomF.Position := 0;
      MPHexEditorEx.LoadFromStream(RomF);
      StatusBar.Panels.Items[2].Text := LabelChipName.Caption;
    end;
    //I2C
    if RadioI2C.Checked then
    begin
      if ComboAddrType.ItemIndex < 0 then
      begin
        LogPrint(STR_CHECK_SETTINGS);
        Exit;
      end;

      EnterProgModeI2c();

      //Адрес микросхемы по чекбоксам
      I2C_DevAddr := SetI2CDevAddr();

      if CheckBox_I2C_ByteRead.Checked then I2C_ChunkSize := 1;

      if UsbAspI2C_BUSY(I2C_DevAddr) then
      begin
        LogPrint(STR_I2C_NO_ANSWER);
        exit;
      end;
      TimeCounter := Time();
      ReadFlashI2C(RomF, Hex2Dec('$'+StartAddressEdit.Text), StrToInt(ComboChipSize.Text), I2C_ChunkSize, I2C_DevAddr);

      RomF.Position := 0;
      MPHexEditorEx.LoadFromStream(RomF);
      StatusBar.Panels.Items[2].Text := LabelChipName.Caption;
    end;
    //Microwire
    if RadioMw.Checked then
    begin
      if (not IsNumber(ComboMWBitLen.Text)) then
      begin
        LogPrint(STR_CHECK_SETTINGS);
        Exit;
      end;

      if not AsProgrammer.Programmer.MWInit(SetSPISpeed(0)) then Exit;
      TimeCounter := Time();
      ReadFlashMW(RomF, StrToInt(ComboMWBitLen.Text), 0, StrToInt(ComboChipSize.Text));

      RomF.Position := 0;
      MPHexEditorEx.LoadFromStream(RomF);
      StatusBar.Panels.Items[2].Text := LabelChipName.Caption;
    end;

    LogPrint(STR_TIME + TimeToStr(Time() - TimeCounter));

    CRC32 := UpdateCRC32($FFFFFFFF, Romf.Memory, Romf.Size);
    LogPrint('CRC32 = 0x'+IntToHex(CRC32, 8));

  finally
    // Gọi hàm ExitProgMode phù hợp dựa trên loại chip đã sử dụng
    if UsedSPICmd = SPI_CMD_25_NAND then
      ExitProgMode25NAND // Gọi hàm NAND
    else if UsedSPICmd in [SPI_CMD_25, SPI_CMD_45, SPI_CMD_95, SPI_CMD_KB] then // Các loại khác dùng 25
      ExitProgMode25; // Gọi hàm NOR
    // I2C và MW có logic riêng, không cần kiểm tra UsedSPICmd

    AsProgrammer.Programmer.DevClose;
    UnlockControl();
  end;
end;

procedure TMainForm.ClearLogMenuItemClick(Sender: TObject);
begin
  Log.Lines.Clear;
end;

procedure TMainForm.ComboSPICMDChange(Sender: TObject);
begin
  RadioSPI.OnChange(Sender);
end;


procedure TMainForm.CopyLogMenuItemClick(Sender: TObject);
begin
  Log.CopyToClipboard;
end;

procedure TMainForm.AllowInsertItemClick(Sender: TObject);
begin
  MPHexEditorEx.NoSizeChange := not AllowInsertItem.Checked;
  MPHexEditorEx.InsertMode := AllowInsertItem.Checked;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  ButtonCancel.Tag := 1;
  ScriptEditForm.FormCloseQuery(Sender, CanClose);
end;


// Thêm hằng số SPI_CMD_25_NAND vào phần const nếu chưa có
// const
//   ...
//   SPI_CMD_25_NAND        = 4; // hoặc giá trị phù hợp với thứ tự trong ComboSPICMD

// Cập nhật hàm ButtonEraseClick
procedure TMainForm.ButtonEraseClick(Sender: TObject);
var
  I2C_DevAddr: byte;
  UsedSPICmd: byte; // Biến để theo dõi loại SPI đang sử dụng
begin
  UsedSPICmd := 255; // Khởi tạo giá trị mặc định không hợp lệ
  try
    ButtonCancel.Tag := 0;
    if not OpenDevice() then exit;
    if Sender <> ComboItem1 then
      if MessageDlg('AsProgrammer', STR_START_ERASE, mtConfirmation, [mbYes, mbNo], 0)
        <> mrYes then Exit;
    LockControl();

    if RunScriptFromFile(CurrentICParam.Script, 'erase') then Exit;

    LogPrint(TimeToStr(Time()));

    //SPI
    if RadioSPI.Checked then
    begin
      // Ghi lại loại chip đang sử dụng để dùng trong finally
      UsedSPICmd := ComboSPICMD.ItemIndex;

      // Vào chế độ lập trình dựa trên loại chip được chọn
      if ComboSPICMD.ItemIndex = SPI_CMD_KB then
        EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked) // KB có thể dùng logic 25?
      else if ComboSPICMD.ItemIndex = SPI_CMD_25_NAND then
        EnterProgMode25NAND(SetSPISpeed(0)) // Gọi hàm NAND
      else // SPI_CMD_25, SPI_CMD_45, SPI_CMD_95
        EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked);

      if ComboSPICMD.ItemIndex <> SPI_CMD_KB then
        IsLockBitsEnabled;
      TimeCounter := Time();

      LogPrint(STR_ERASING_FLASH);

      if ComboSPICMD.ItemIndex = SPI_CMD_KB then
      begin
        if (not IsNumber(ComboChipSize.Text)) or (not IsNumber(ComboPageSize.Text)) then
        begin
          LogPrint(STR_CHECK_SETTINGS);
          Exit;
        end;
        EraseFlashKB(StrToInt(ComboChipSize.Text), StrToInt(ComboPageSize.Text));
      end
      else if ComboSPICMD.ItemIndex = SPI_CMD_25 then
      begin
        UsbAsp25_WREN();
        UsbAsp25_ChipErase();

        ProgressBar.Style:= pbstMarquee;
        ProgressBar.Max:= 1;
        ProgressBar.Position:= 1;

        LogPrint(STR_ERASE_NOTICE);

        while UsbAsp25_Busy() do
        begin
          Application.ProcessMessages;
          if UserCancel then Exit;
        end;

        ProgressBar.Style:= pbstNormal;
        ProgressBar.Position:= 0;
      end
      else if ComboSPICMD.ItemIndex = SPI_CMD_25_NAND then // Thêm nhánh cho NAND
      begin
        // if ( (not IsNumber(ComboChipSize.Text)) or (not IsNumber(ComboPageSize.Text))) then
        //begin
        //  LogPrint(STR_CHECK_SETTINGS);
        //  Exit;
        //end;
        // SPI NAND thường không có lệnh xóa chip đơn lẻ, nên xóa từng block
        // Gọi hàm xóa chip từ spi25NAND.pas (giả sử hàm này đã được cập nhật để xóa toàn chip)
        if UsbAsp25NAND_ChipErase() = 0 then // Giả sử trả về 0 là thành công
        begin
          LogPrint(STR_DONE);
          // Có thể không cần vòng lặp busy như NOR, hoặc cần theo dõi theo block
          // Nếu chip erase thực hiện block-by-block bên trong UsbAsp25NAND_ChipErase,
          // thì không cần vòng lặp busy ở đây.
          // Nếu cần theo dõi theo block, thì cần thay đổi cách cập nhật ProgressBar.
        end
        else
        begin
          LogPrint('Chip Erase Failed');
          // Có thể cần Exit hoặc xử lý lỗi khác
        end;
      end
      else if ComboSPICMD.ItemIndex = SPI_CMD_95 then
      begin
        if ( (not IsNumber(ComboChipSize.Text)) or (not IsNumber(ComboPageSize.Text))) then
        begin
          LogPrint(STR_CHECK_SETTINGS);
          Exit;
        end;
        EraseEEPROM25(0, StrToInt(ComboChipSize.Text), StrToInt(ComboPageSize.Text), StrToInt(ComboChipSize.Text));
      end
      else if ComboSPICMD.ItemIndex = SPI_CMD_45 then
      begin
        UsbAsp45_ChipErase();
        while UsbAsp45_Busy() do
        begin
          Application.ProcessMessages;
          if UserCancel then Exit;
        end;
      end;

    end;

    //I2C
    if RadioI2C.Checked then
    begin
      if ( (ComboAddrType.ItemIndex < 0) or (not IsNumber(ComboPageSize.Text)) ) then
      begin
        LogPrint(STR_CHECK_SETTINGS);
        Exit;
      end;

      EnterProgModeI2C();

      //Адрес микросхемы по чекбоксам
      I2C_DevAddr := SetI2CDevAddr();

      if UsbAspI2C_BUSY(I2C_DevAddr) then
      begin
        LogPrint(STR_I2C_NO_ANSWER);
        exit;
      end;

      TimeCounter := Time();

      if StrToInt(ComboPageSize.Text) < 1 then ComboPageSize.Text := '1';

      EraseFlashI2C(0, StrToInt(ComboChipSize.Text), StrToInt(ComboPageSize.Text), I2C_DevAddr);
    end;

    //Microwire
    if RadioMW.Checked then
    begin
      if (not IsNumber(ComboMWBitLen.Text)) then
      begin
        LogPrint(STR_CHECK_SETTINGS);
        Exit;
      end;

      AsProgrammer.Programmer.MWInit(SetSPISpeed(0));
      TimeCounter := Time();
      LogPrint(STR_ERASING_FLASH);
      UsbAspMW_Ewen(StrToInt(ComboMWBitLen.Text));
      UsbAspMW_ChipErase(StrToInt(ComboMWBitLen.Text));

       while UsbAspMW_Busy do
       begin
         Application.ProcessMessages;
         if UserCancel then Exit;
       end;

    end;

    // LogPrint(STR_DONE); // Đã được gọi trong từng hàm xóa cụ thể
    LogPrint(STR_TIME + TimeToStr(Time() - TimeCounter));

  finally
    // Gọi hàm ExitProgMode phù hợp dựa trên loại chip đã sử dụng
    if UsedSPICmd = SPI_CMD_25_NAND then
      ExitProgMode25NAND // Gọi hàm NAND
    else if UsedSPICmd in [SPI_CMD_25, SPI_CMD_45, SPI_CMD_95, SPI_CMD_KB] then // Các loại khác dùng 25
      ExitProgMode25; // Gọi hàm NOR
    // I2C và MW có logic riêng, không cần kiểm tra UsedSPICmd

    AsProgrammer.Programmer.DevClose;
    UnlockControl();
  end;
end;

procedure TMainForm.BlankCheckMenuItemClick(Sender: TObject);
begin
  VerifyFlash(true);
end;

procedure SaveOptions(XMLfile: TXMLDocument);
var
  Node, ParentNode: TDOMNode;
begin
  if XMLfile <> nil then
  begin
    //Удаляем старую запись
    Node := XMLfile.DocumentElement.FindNode('locale');
    if (Node <> nil) then XMLfile.DocumentElement.RemoveChild(Node);
    //Создаем новую
    Node:= XMLfile.DocumentElement;
    ParentNode := XMLfile.CreateElement('locale');
    TDOMElement(ParentNode).SetAttribute('lang', CurrentLang);
    Node.Appendchild(parentNode);

    //Удаляем старую запись
    Node := XMLfile.DocumentElement.FindNode('options');
    if (Node <> nil) then XMLfile.DocumentElement.RemoveChild(Node);

    Node:= XMLfile.DocumentElement;
    ParentNode := XMLfile.CreateElement('options');

    if MainForm.MenuAutoCheck.Checked then
      TDOMElement(ParentNode).SetAttribute('verify', '1') else
        TDOMElement(ParentNode).SetAttribute('verify', '0');

    if MainForm.MenuSkipFF.Checked then
      TDOMElement(ParentNode).SetAttribute('skipff', '1') else
        TDOMElement(ParentNode).SetAttribute('skipff', '0');

    if MainForm.MenuSendAB.Checked then
      TDOMElement(ParentNode).SetAttribute('sendab', '1') else
        TDOMElement(ParentNode).SetAttribute('sendab', '0');

    if MainForm.Menu3Mhz.Checked then
      TDOMElement(ParentNode).SetAttribute('spi_speed', '3Mhz');
    if MainForm.Menu1_5Mhz.Checked then
      TDOMElement(ParentNode).SetAttribute('spi_speed', '1_5Mhz');
    if MainForm.Menu750Khz.Checked then
      TDOMElement(ParentNode).SetAttribute('spi_speed', '750Khz');
    if MainForm.Menu375Khz.Checked then
      TDOMElement(ParentNode).SetAttribute('spi_speed', '375Khz');
    if MainForm.Menu187_5Khz.Checked then
      TDOMElement(ParentNode).SetAttribute('spi_speed', '187_5Khz');
    if MainForm.Menu93_75Khz.Checked then
      TDOMElement(ParentNode).SetAttribute('spi_speed', '93_75Khz');
    if MainForm.Menu32Khz.Checked then
      TDOMElement(ParentNode).SetAttribute('spi_speed', '32Khz');

    if MainForm.MenuCH347SPIClock60MHz.Checked then
      TDOMElement(ParentNode).SetAttribute('ch347_spi_speed', '60Mhz');
    if MainForm.MenuCH347SPIClock30MHz.Checked then
      TDOMElement(ParentNode).SetAttribute('ch347_spi_speed', '30Mhz');
    if MainForm.MenuCH347SPIClock15MHz.Checked then
      TDOMElement(ParentNode).SetAttribute('ch347_spi_speed', '15Mhz');
    if MainForm.MenuCH347SPIClock7_5MHz.Checked then
      TDOMElement(ParentNode).SetAttribute('ch347_spi_speed', '7_5Mhz');
    if MainForm.MenuCH347SPIClock3_75MHz.Checked then
      TDOMElement(ParentNode).SetAttribute('ch347_spi_speed', '3_75Mhz');
    if MainForm.MenuCH347SPIClock1_875MHz.Checked then
      TDOMElement(ParentNode).SetAttribute('ch347_spi_speed', '1_875MHz');
    if MainForm.MenuCH347SPIClock937_5KHz.Checked then
      TDOMElement(ParentNode).SetAttribute('ch347_spi_speed', '937_5KHz');
    if MainForm.MenuCH347SPIClock468_75KHz.Checked then
      TDOMElement(ParentNode).SetAttribute('ch347_spi_speed', '468_75KHz');

    if MainForm.MenuMW32Khz.Checked then
      TDOMElement(ParentNode).SetAttribute('mw_speed', '32Khz');
    if MainForm.MenuMW16Khz.Checked then
      TDOMElement(ParentNode).SetAttribute('mw_speed', '16Khz');
    if MainForm.MenuMW8Khz.Checked then
      TDOMElement(ParentNode).SetAttribute('mw_speed', '8Khz');

    if MainForm.MenuHWUSBASP.Checked then
      TDOMElement(ParentNode).SetAttribute('hw', 'usbasp');
    if MainForm.MenuHWCH341A.Checked then
      TDOMElement(ParentNode).SetAttribute('hw', 'ch341a');
    if MainForm.MenuHWCH347.Checked then
      TDOMElement(ParentNode).SetAttribute('hw', 'ch347');
    if MainForm.MenuHWAVRISP.Checked then
      TDOMElement(ParentNode).SetAttribute('hw', 'avrisp');
    if MainForm.MenuHWARDUINO.Checked then
      TDOMElement(ParentNode).SetAttribute('hw', 'arduino');
    if MainForm.MenuHWFT232H.Checked then
      TDOMElement(ParentNode).SetAttribute('hw', 'ft232h');

    TDOMElement(ParentNode).SetAttribute('arduino_comport', Arduino_COMPort);
    TDOMElement(ParentNode).SetAttribute('arduino_baudrate', IntToStr(Arduino_BaudRate));

    Node.Appendchild(parentNode);

    WriteXMLFile(XMLfile, SettingsFileName);
  end;

end;

procedure LoadOptions(XMLfile: TXMLDocument);
var
    Node: TDOMNode;
    OptVal: string;
begin
  if XMLfile <> nil then
  begin
    Node := XMLfile.DocumentElement.FindNode('options');

    if (Node <> nil) then
    if (Node.HasAttributes) then
    begin

      if  Node.Attributes.GetNamedItem('verify') <> nil then
      begin
        if Node.Attributes.GetNamedItem('verify').NodeValue = '1' then
          MainForm.MenuAutoCheck.Checked := true;
      end;

      if  Node.Attributes.GetNamedItem('sendab') <> nil then
      begin
        if Node.Attributes.GetNamedItem('sendab').NodeValue = '1' then
          MainForm.MenuSendAB.Checked := true;
      end;

      if  Node.Attributes.GetNamedItem('skipff') <> nil then
      begin
        if Node.Attributes.GetNamedItem('skipff').NodeValue = '1' then
          MainForm.MenuSkipFF.Checked := true;
      end;

      if  Node.Attributes.GetNamedItem('spi_speed') <> nil then
      begin
        OptVal := UTF16ToUTF8(Node.Attributes.GetNamedItem('spi_speed').NodeValue);

        if OptVal = '3Mhz' then MainForm.Menu3Mhz.Checked := true;
        if OptVal = '1_5Mhz' then MainForm.Menu1_5Mhz.Checked := true;
        if OptVal = '750Khz' then MainForm.Menu750Khz.Checked := true;
        if OptVal = '375Khz' then MainForm.Menu375Khz.Checked := true;
        if OptVal = '187_5Khz' then MainForm.Menu187_5Khz.Checked := true;
        if OptVal = '93_75Khz' then MainForm.Menu93_75Khz.Checked := true;
        if OptVal = '32Khz' then MainForm.Menu32Khz.Checked := true;
      end;

      if  Node.Attributes.GetNamedItem('ch347_spi_speed') <> nil then
      begin
        OptVal := UTF16ToUTF8(Node.Attributes.GetNamedItem('ch347_spi_speed').NodeValue);

        if OptVal = '60Mhz' then MainForm.MenuCH347SPIClock60MHz.Checked := true;
        if OptVal = '30Mhz' then MainForm.MenuCH347SPIClock30MHz.Checked := true;
        if OptVal = '15Mhz' then MainForm.MenuCH347SPIClock15MHz.Checked := true;
        if OptVal = '7_5Mhz' then MainForm.MenuCH347SPIClock7_5MHz.Checked := true;
        if OptVal = '3_75Mhz' then MainForm.MenuCH347SPIClock3_75MHz.Checked := true;
        if OptVal = '1_875MHz' then MainForm.MenuCH347SPIClock1_875MHz.Checked := true;
        if OptVal = '937_5KHz' then MainForm.MenuCH347SPIClock937_5KHz.Checked := true;
        if OptVal = '468_75KHz' then MainForm.MenuCH347SPIClock468_75KHz.Checked := true;
      end;


      if  Node.Attributes.GetNamedItem('mw_speed') <> nil then
      begin
        OptVal := UTF16ToUTF8(Node.Attributes.GetNamedItem('mw_speed').NodeValue);

        if OptVal = '32Khz' then MainForm.MenuMW32Khz.Checked := true;
        if OptVal = '16Khz' then MainForm.MenuMW16Khz.Checked := true;
        if OptVal = '8Khz' then MainForm.MenuMW8Khz.Checked := true;
      end;

      if  Node.Attributes.GetNamedItem('hw') <> nil then
      begin
        OptVal := UTF16ToUTF8(Node.Attributes.GetNamedItem('hw').NodeValue);

        if OptVal = 'usbasp' then
        begin
          MainForm.MenuHWUSBASP.Checked := true;
          SelectHW(CHW_USBASP);
        end;

        if OptVal = 'ch341a' then
        begin
          MainForm.MenuHWCH341A.Checked := true;
          SelectHW(CHW_CH341);
        end;

        if OptVal = 'ch347' then
        begin
          MainForm.MenuHWCH347.Checked := true;
          SelectHW(CHW_CH347);
        end;

        if OptVal = 'avrisp' then
        begin
          MainForm.MenuHWAVRISP.Checked := true;
          SelectHW(CHW_AVRISP);
        end;

        if OptVal = 'arduino' then
        begin
          MainForm.MenuHWArduino.Checked := true;
          SelectHW(CHW_ARDUINO);
        end;

        if OptVal = 'ft232h' then
        begin
          MainForm.MenuHWFT232H.Checked := true;
          SelectHW(CHW_FT232H);
        end;


      end;

      if  Node.Attributes.GetNamedItem('arduino_comport') <> nil then
      begin
        OptVal := UTF16ToUTF8(Node.Attributes.GetNamedItem('arduino_comport').NodeValue);

        Arduino_COMPort := OptVal;
        MainForm.MenuArduinoCOMPort.Caption := 'Arduino COMPort: '+ Arduino_COMPort;
      end;

      if  Node.Attributes.GetNamedItem('arduino_baudrate') <> nil then
      begin
        OptVal := UTF16ToUTF8(Node.Attributes.GetNamedItem('arduino_baudrate').NodeValue);

        Arduino_BaudRate := StrToInt(OptVal);
      end;

    end;
  end;

end;


end.
