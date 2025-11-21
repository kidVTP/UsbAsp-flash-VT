unit sregedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TSregTypeList = (ST_MACRONIX, ST_WINBOND, ST_GIGADEVICE);

  { TsregeditForm }

  TsregeditForm = class(TForm)
    ButtonReadSreg: TButton;
    ButtonWriteSreg: TButton;
    CheckBoxSB0: TCheckBox;
    CheckBoxSB1: TCheckBox;
    CheckBoxSB10: TCheckBox;
    CheckBoxSB11: TCheckBox;
    CheckBoxSB12: TCheckBox;
    CheckBoxSB13: TCheckBox;
    CheckBoxSB14: TCheckBox;
    CheckBoxSB15: TCheckBox;
    CheckBoxSB16: TCheckBox;
    CheckBoxSB17: TCheckBox;
    CheckBoxSB18: TCheckBox;
    CheckBoxSB19: TCheckBox;
    CheckBoxSB2: TCheckBox;
    CheckBoxSB20: TCheckBox;
    CheckBoxSB21: TCheckBox;
    CheckBoxSB22: TCheckBox;
    CheckBoxSB23: TCheckBox;
    CheckBoxSB3: TCheckBox;
    CheckBoxSB4: TCheckBox;
    CheckBoxSB5: TCheckBox;
    CheckBoxSB6: TCheckBox;
    CheckBoxSB7: TCheckBox;
    CheckBoxSB8: TCheckBox;
    CheckBoxSB9: TCheckBox;
    ComboBoxSRType: TComboBox;
    GroupBoxSREG2: TGroupBox;
    GroupBoxSREG3: TGroupBox;
    GroupBoxSREG1: TGroupBox;
    // --- Thêm các thành phần UI cho SPI NAND ---
    GroupBoxNANDSREG1: TGroupBox;
    GroupBoxNANDSREG2: TGroupBox;
    GroupBoxNANDSREG3: TGroupBox;
    GroupBoxNANDSpecial: TGroupBox;
    CheckBoxNANDSR0: TCheckBox;
    CheckBoxNANDSR1: TCheckBox;
    CheckBoxNANDSR2: TCheckBox;
    CheckBoxNANDSR3: TCheckBox;
    CheckBoxNANDSR4: TCheckBox;
    CheckBoxNANDSR5: TCheckBox;
    CheckBoxNANDSR6: TCheckBox;
    CheckBoxNANDSR7: TCheckBox;
    CheckBoxNANDPR0: TCheckBox;
    CheckBoxNANDPR1: TCheckBox;
    CheckBoxNANDPR2: TCheckBox;
    CheckBoxNANDPR3: TCheckBox;
    CheckBoxNANDPR4: TCheckBox;
    CheckBoxNANDPR5: TCheckBox;
    CheckBoxNANDPR6: TCheckBox;
    CheckBoxNANDPR7: TCheckBox;
    CheckBoxNANDCR0: TCheckBox;
    CheckBoxNANDCR1: TCheckBox;
    CheckBoxNANDCR2: TCheckBox;
    CheckBoxNANDCR3: TCheckBox;
    CheckBoxNANDCR4: TCheckBox;
    CheckBoxNANDCR5: TCheckBox;
    CheckBoxNANDCR6: TCheckBox;
    CheckBoxNANDCR7: TCheckBox;
    ButtonUnprotectNAND: TButton;
    ButtonReadBBTable: TButton;
    // ---
    procedure ButtonReadSregClick(Sender: TObject);
    procedure ButtonWriteSregClick(Sender: TObject);
    procedure ComboBoxSRTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    // --- Thêm sự kiện cho các nút NAND ---
    procedure ButtonUnprotectNANDClick(Sender: TObject);
    procedure ButtonReadBBTableClick(Sender: TObject);
    // ---
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  sregeditForm: TsregeditForm;
  SREGType: TSregTypeList;

implementation

uses main, spi25, spi25nand, msgstr, utilfunc; // Thêm spi25nand

{$R *.lfm}

// --- Hàm tiện ích để chuyển đổi giữa byte và checkbox ---
procedure SetSreg1CheckBox(sreg1: byte);
begin
  with sregeditForm do
  begin
    CheckBoxSB0.Checked:= IsBitSet(sreg1, 0);
    CheckBoxSB1.Checked:= IsBitSet(sreg1, 1);
    CheckBoxSB2.Checked:= IsBitSet(sreg1, 2);
    CheckBoxSB3.Checked:= IsBitSet(sreg1, 3);
    CheckBoxSB4.Checked:= IsBitSet(sreg1, 4);
    CheckBoxSB5.Checked:= IsBitSet(sreg1, 5);
    CheckBoxSB6.Checked:= IsBitSet(sreg1, 6);
    CheckBoxSB7.Checked:= IsBitSet(sreg1, 7);
  end;
end;

function GetSreg1CheckBox(): byte;
begin
  with sregeditForm do
  begin
    result := 0;
    if CheckBoxSB0.Checked then result := SetBit(result, 0);
    if CheckBoxSB1.Checked then result := SetBit(result, 1);
    if CheckBoxSB2.Checked then result := SetBit(result, 2);
    if CheckBoxSB3.Checked then result := SetBit(result, 3);
    if CheckBoxSB4.Checked then result := SetBit(result, 4);
    if CheckBoxSB5.Checked then result := SetBit(result, 5);
    if CheckBoxSB6.Checked then result := SetBit(result, 6);
    if CheckBoxSB7.Checked then result := SetBit(result, 7);
  end;
end;

procedure SetSreg2CheckBox(sreg2: byte);
begin
  with sregeditForm do
  begin
    CheckBoxSB8.Checked:= IsBitSet(sreg2, 0);
    CheckBoxSB9.Checked:= IsBitSet(sreg2, 1);
    CheckBoxSB10.Checked:= IsBitSet(sreg2, 2);
    CheckBoxSB11.Checked:= IsBitSet(sreg2, 3);
    CheckBoxSB12.Checked:= IsBitSet(sreg2, 4);
    CheckBoxSB13.Checked:= IsBitSet(sreg2, 5);
    CheckBoxSB14.Checked:= IsBitSet(sreg2, 6);
    CheckBoxSB15.Checked:= IsBitSet(sreg2, 7);
  end;
end;

function GetSreg2CheckBox(): byte;
begin
  with sregeditForm do
  begin
    result := 0;
    if CheckBoxSB8.Checked then result := SetBit(result, 0);
    if CheckBoxSB9.Checked then result := SetBit(result, 1);
    if CheckBoxSB10.Checked then result := SetBit(result, 2);
    if CheckBoxSB11.Checked then result := SetBit(result, 3);
    if CheckBoxSB12.Checked then result := SetBit(result, 4);
    if CheckBoxSB13.Checked then result := SetBit(result, 5);
    if CheckBoxSB14.Checked then result := SetBit(result, 6);
    if CheckBoxSB15.Checked then result := SetBit(result, 7);
  end;
end;

procedure SetSreg3CheckBox(sreg3: byte);
begin
  with sregeditForm do
  begin
    CheckBoxSB16.Checked:= IsBitSet(sreg3, 0);
    CheckBoxSB17.Checked:= IsBitSet(sreg3, 1);
    CheckBoxSB18.Checked:= IsBitSet(sreg3, 2);
    CheckBoxSB19.Checked:= IsBitSet(sreg3, 3);
    CheckBoxSB20.Checked:= IsBitSet(sreg3, 4);
    CheckBoxSB21.Checked:= IsBitSet(sreg3, 5);
    CheckBoxSB22.Checked:= IsBitSet(sreg3, 6);
    CheckBoxSB23.Checked:= IsBitSet(sreg3, 7);
  end;
end;

function GetSreg3CheckBox(): byte;
begin
  with sregeditForm do
  begin
    result := 0;
    if CheckBoxSB16.Checked then result := SetBit(result, 0);
    if CheckBoxSB17.Checked then result := SetBit(result, 1);
    if CheckBoxSB18.Checked then result := SetBit(result, 2);
    if CheckBoxSB19.Checked then result := SetBit(result, 3);
    if CheckBoxSB20.Checked then result := SetBit(result, 4);
    if CheckBoxSB21.Checked then result := SetBit(result, 5);
    if CheckBoxSB22.Checked then result := SetBit(result, 6);
    if CheckBoxSB23.Checked then result := SetBit(result, 7);
  end;
end;

// --- Hàm tiện ích mới cho SPI NAND ---
procedure SetNANDSregCheckBoxes(sreg1, sreg2, sreg3: byte);
begin
  with sregeditForm do
  begin
    // Status Register ($C0)
    CheckBoxNANDSR0.Checked := IsBitSet(sreg1, 0);
    CheckBoxNANDSR1.Checked := IsBitSet(sreg1, 1);
    CheckBoxNANDSR2.Checked := IsBitSet(sreg1, 2);
    CheckBoxNANDSR3.Checked := IsBitSet(sreg1, 3);
    CheckBoxNANDSR4.Checked := IsBitSet(sreg1, 4);
    CheckBoxNANDSR5.Checked := IsBitSet(sreg1, 5);
    CheckBoxNANDSR6.Checked := IsBitSet(sreg1, 6);
    CheckBoxNANDSR7.Checked := IsBitSet(sreg1, 7);

    // Protection Register ($A0)
    CheckBoxNANDPR0.Checked := IsBitSet(sreg2, 0);
    CheckBoxNANDPR1.Checked := IsBitSet(sreg2, 1);
    CheckBoxNANDPR2.Checked := IsBitSet(sreg2, 2);
    CheckBoxNANDPR3.Checked := IsBitSet(sreg2, 3);
    CheckBoxNANDPR4.Checked := IsBitSet(sreg2, 4);
    CheckBoxNANDPR5.Checked := IsBitSet(sreg2, 5);
    CheckBoxNANDPR6.Checked := IsBitSet(sreg2, 6);
    CheckBoxNANDPR7.Checked := IsBitSet(sreg2, 7);

    // Config Register ($B0)
    CheckBoxNANDCR0.Checked := IsBitSet(sreg3, 0);
    CheckBoxNANDCR1.Checked := IsBitSet(sreg3, 1);
    CheckBoxNANDCR2.Checked := IsBitSet(sreg3, 2);
    CheckBoxNANDCR3.Checked := IsBitSet(sreg3, 3);
    CheckBoxNANDCR4.Checked := IsBitSet(sreg3, 4);
    CheckBoxNANDCR5.Checked := IsBitSet(sreg3, 5);
    CheckBoxNANDCR6.Checked := IsBitSet(sreg3, 6);
    CheckBoxNANDCR7.Checked := IsBitSet(sreg3, 7);
  end;
end;

function GetNANDSreg1CheckBoxes(): byte; // Status Reg
begin
  with sregeditForm do
  begin
    result := 0;
    if CheckBoxNANDSR0.Checked then result := SetBit(result, 0);
    if CheckBoxNANDSR1.Checked then result := SetBit(result, 1);
    if CheckBoxNANDSR2.Checked then result := SetBit(result, 2);
    if CheckBoxNANDSR3.Checked then result := SetBit(result, 3);
    if CheckBoxNANDSR4.Checked then result := SetBit(result, 4);
    if CheckBoxNANDSR5.Checked then result := SetBit(result, 5);
    if CheckBoxNANDSR6.Checked then result := SetBit(result, 6);
    if CheckBoxNANDSR7.Checked then result := SetBit(result, 7);
  end;
end;

function GetNANDSreg2CheckBoxes(): byte; // Protection Reg
begin
  with sregeditForm do
  begin
    result := 0;
    if CheckBoxNANDPR0.Checked then result := SetBit(result, 0);
    if CheckBoxNANDPR1.Checked then result := SetBit(result, 1);
    if CheckBoxNANDPR2.Checked then result := SetBit(result, 2);
    if CheckBoxNANDPR3.Checked then result := SetBit(result, 3);
    if CheckBoxNANDPR4.Checked then result := SetBit(result, 4);
    if CheckBoxNANDPR5.Checked then result := SetBit(result, 5);
    if CheckBoxNANDPR6.Checked then result := SetBit(result, 6);
    if CheckBoxNANDPR7.Checked then result := SetBit(result, 7);
  end;
end;

function GetNANDSreg3CheckBoxes(): byte; // Config Reg
begin
  with sregeditForm do
  begin
    result := 0;
    if CheckBoxNANDCR0.Checked then result := SetBit(result, 0);
    if CheckBoxNANDCR1.Checked then result := SetBit(result, 1);
    if CheckBoxNANDCR2.Checked then result := SetBit(result, 2);
    if CheckBoxNANDCR3.Checked then result := SetBit(result, 3);
    if CheckBoxNANDCR4.Checked then result := SetBit(result, 4);
    if CheckBoxNANDCR5.Checked then result := SetBit(result, 5);
    if CheckBoxNANDCR6.Checked then result := SetBit(result, 6);
    if CheckBoxNANDCR7.Checked then result := SetBit(result, 7);
  end;
end;

// --- Cập nhật các hàm xử lý sự kiện ---
procedure TsregeditForm.ButtonReadSregClick(Sender: TObject);
var
  sreg1, sreg2, sreg3: byte; // Dùng chung cho cả NOR và NAND
  nand_sreg1, nand_sreg2, nand_sreg3: byte; // Dành riêng cho NAND
  SPI_CMD_Type: Integer; // Biến để xác định loại SPI từ MainForm
begin
  // Lấy loại SPI từ MainForm
  SPI_CMD_Type := MainForm.ComboSPICMD.ItemIndex;

  if SPI_CMD_Type = SPI_CMD_25 then
  begin
    // Logic đọc SREG cho SPI NOR (giữ nguyên)
    try
      if not OpenDevice() then exit;
      EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked);

      UsbAsp25_ReadSR(sreg1);

      if SREGType = ST_MACRONIX then
      begin
        UsbAsp25_ReadSR(sreg2, $15);
        sreg3 := 0;
      end
      else if (SREGType = ST_WINBOND) or (SREGType = ST_GIGADEVICE) then
      begin
        UsbAsp25_ReadSR(sreg2, $35);
        UsbAsp25_ReadSR(sreg3, $15);
      end;

      SetSreg1CheckBox(sreg1);
      SetSreg2CheckBox(sreg2);
      SetSreg3CheckBox(sreg3);

      //// Ẩn các GroupBox của NAND
      //GroupBoxNANDSREG1.Visible := False;
      //GroupBoxNANDSREG2.Visible := False;
      //GroupBoxNANDSREG3.Visible := False;
      //GroupBoxNANDSpecial.Visible := False; // Ẩn nhóm đặc biệt
      //// Hiện các GroupBox của NOR
      //GroupBoxSREG1.Visible := True;
      //GroupBoxSREG2.Visible := True;
      //GroupBoxSREG3.Visible := True;

    finally
      ExitProgMode25;
      AsProgrammer.Programmer.DevClose;
    end;
  end
  else if SPI_CMD_Type = SPI_CMD_25_NAND then // Thêm nhánh cho NAND
  begin
    try
      if not OpenDevice() then exit;
      EnterProgMode25NAND(SetSPISpeed(0)); // Gọi hàm vào chế độ NAND

      // Đọc các Feature Register cho NAND
      if UsbAsp25NAND_ReadStatus(nand_sreg1, SPI_NAND_FEATURE_STATUS) < 0 then
      begin
        LogPrint('Error reading SPI NAND Status Register ($C0).');
        nand_sreg1 := $FF; // Gán giá trị mặc định nếu lỗi
      end;

      if UsbAsp25NAND_ReadStatus(nand_sreg2, SPI_NAND_FEATURE_PROTECTION) < 0 then
      begin
        LogPrint('Error reading SPI NAND Protection Register ($A0).');
        nand_sreg2 := $FF; // Gán giá trị mặc định nếu lỗi
      end;

      if UsbAsp25NAND_ReadStatus(nand_sreg3, SPI_NAND_FEATURE_CONFIG) < 0 then
      begin
        LogPrint('Error reading SPI NAND Config Register ($B0).');
        nand_sreg3 := $FF; // Gán giá trị mặc định nếu lỗi
      end;

      // Hiển thị giá trị đọc được lên các checkbox
      SetNANDSregCheckBoxes(nand_sreg1, nand_sreg2, nand_sreg3);

      //// Ẩn các GroupBox của NOR
      //GroupBoxSREG1.Visible := False;
      //GroupBoxSREG2.Visible := False;
      //GroupBoxSREG3.Visible := False;
      //// Hiện các GroupBox của NAND
      //GroupBoxNANDSREG1.Visible := True;
      //GroupBoxNANDSREG2.Visible := True;
      //GroupBoxNANDSREG3.Visible := True;
      //GroupBoxNANDSpecial.Visible := True; // Hiện nhóm đặc biệt

    finally
      ExitProgMode25NAND; // Gọi hàm ra chế độ NAND
      AsProgrammer.Programmer.DevClose;
    end;
  end
  else
  begin
    LogPrint('Unsupported SPI command type for SREG edit.');
  end;
end;

procedure TsregeditForm.ButtonWriteSregClick(Sender: TObject);
var
  sreg1, sreg2, sreg3: byte; // Dành cho NOR
  nand_sreg1, nand_sreg2, nand_sreg3: byte; // Dành cho NAND
  SPI_CMD_Type: Integer; // Biến để xác định loại SPI từ MainForm
begin
  SPI_CMD_Type := MainForm.ComboSPICMD.ItemIndex;

  if SPI_CMD_Type = SPI_CMD_25 then
  begin
    // Logic ghi SREG cho SPI NOR (giữ nguyên)
    try
      if not OpenDevice() then exit;
      EnterProgMode25(SetSPISpeed(0), MainForm.MenuSendAB.Checked);

      UsbAsp25_WREN();
      sreg1 := GetSreg1CheckBox();
      UsbAsp25_WriteSR(sreg1);

      while UsbAsp25_Busy() do
      begin
        Application.ProcessMessages;
        if UserCancel then Exit;
      end;

      if SREGType = ST_MACRONIX then
      begin
        UsbAsp25_WREN();
        sreg2 := GetSreg2CheckBox();
        UsbAsp25_WriteSR_2byte(sreg1, sreg2); // Hoặc chỉ ghi thanh ghi 2 nếu cần

        while UsbAsp25_Busy() do
        begin
          Application.ProcessMessages;
          if UserCancel then Exit;
        end;
      end
      else if (SREGType = ST_WINBOND) or (SREGType = ST_GIGADEVICE) then
      begin
        UsbAsp25_WREN();
        sreg2 := GetSreg2CheckBox();
        UsbAsp25_WriteSR(sreg2, $31); // Ghi SREG2

        while UsbAsp25_Busy() do
        begin
          Application.ProcessMessages;
          if UserCancel then Exit;
        end;

        UsbAsp25_WREN();
        sreg3 := GetSreg3CheckBox();
        UsbAsp25_WriteSR(sreg3, $11); // Ghi SREG3

        while UsbAsp25_Busy() do
        begin
          Application.ProcessMessages;
          if UserCancel then Exit;
        end;
      end;

    finally
      ExitProgMode25;
      AsProgrammer.Programmer.DevClose;
    end;
  end
  else if SPI_CMD_Type = SPI_CMD_25_NAND then // Thêm nhánh cho NAND
  begin
    try
      if not OpenDevice() then exit;
      EnterProgMode25NAND(SetSPISpeed(0)); // Gọi hàm vào chế độ NAND

      // Lấy giá trị từ các checkbox của NAND
      nand_sreg1 := GetNANDSreg1CheckBoxes(); // Status - thường không ghi
      nand_sreg2 := GetNANDSreg2CheckBoxes(); // Protection
      nand_sreg3 := GetNANDSreg3CheckBoxes(); // Config

      // Ghi các Feature Register cho NAND
      // Ghi Status Register ($C0) thường không được phép hoặc có ý nghĩa hạn chế
      // LogPrint('Writing Status Reg ($C0): ' + IntToHex(nand_sreg1, 2)); // Log để debug
      // UsbAsp25NAND_WriteConfigStatus(nand_sreg1, SPI_NAND_FEATURE_STATUS); // Có thể không hỗ trợ hoặc nguy hiểm

      LogPrint('Writing Protection Reg ($A0): ' + IntToHex(nand_sreg2, 2));
      if UsbAsp25NAND_WriteConfigStatus(nand_sreg2, SPI_NAND_FEATURE_PROTECTION) < 0 then
      begin
         LogPrint('Error writing SPI NAND Protection Register ($A0).');
         // Có thể muốn Exit hoặc tiếp tục
      end;

      LogPrint('Writing Config Reg ($B0): ' + IntToHex(nand_sreg3, 2));
      if UsbAsp25NAND_WriteConfigStatus(nand_sreg3, SPI_NAND_FEATURE_CONFIG) < 0 then
      begin
         LogPrint('Error writing SPI NAND Config Register ($B0).');
         // Có thể muốn Exit hoặc tiếp tục
      end;

      // Không cần vòng lặp Busy ở đây vì WriteConfigStatus đã có xác minh

    finally
      ExitProgMode25NAND; // Gọi hàm ra chế độ NAND
      AsProgrammer.Programmer.DevClose;
    end;
  end
  else
  begin
    LogPrint('Unsupported SPI command type for SREG edit.');
  end;
end;

procedure TsregeditForm.ComboBoxSRTypeChange(Sender: TObject);
begin
  if UpCase(ComboBoxSRType.Text) = 'WINBOND' then
  begin
    SREGType := ST_WINBOND;
    GroupBoxSREG1.Enabled:= true;
    GroupBoxSREG2.Enabled:= true;
    GroupBoxSREG3.Enabled:= true;
  end
  else if UpCase(ComboBoxSRType.Text) = 'GIGADEVICE' then
  begin
    SREGType := ST_GIGADEVICE;
    GroupBoxSREG1.Enabled:= true;
    GroupBoxSREG2.Enabled:= true;
    GroupBoxSREG3.Enabled:= true;
  end
  else if UpCase(ComboBoxSRType.Text) = 'MACRONIX' then
  begin
    SREGType := ST_MACRONIX;
    GroupBoxSREG1.Enabled:= true;
    GroupBoxSREG2.Enabled:= true;
    GroupBoxSREG3.Enabled:= false; // SREG3 bị vô hiệu hóa cho Macronix
  end;
end;


procedure TsregeditForm.FormShow(Sender: TObject);
var
  SPI_CMD_Type: Integer;
begin
  // Lấy loại SPI từ MainForm
  SPI_CMD_Type := MainForm.ComboSPICMD.ItemIndex;

  if SPI_CMD_Type = SPI_CMD_25 then
  begin
    // Hiển thị UI cho SPI NOR
    // Gọi lại hàm cũ để xử lý các GroupBox NOR và ComboBoxSRType
    ComboBoxSRTypeChange(Sender); // <-- Cập nhật UI NOR dựa trên ComboBoxSRType

    // Ẩn các GroupBox của NAND
    GroupBoxNANDSREG1.Visible := False;
    GroupBoxNANDSREG2.Visible := False;
    GroupBoxNANDSREG3.Visible := False;
    GroupBoxNANDSpecial.Visible := False;
    // Hiện các GroupBox của NOR (đã được xử lý trong ComboBoxSRTypeChange)
    // GroupBoxSREG1.Visible := True; // (Ví dụ, thực tế do ComboBoxSRTypeChange xử lý)
    // GroupBoxSREG2.Visible := True;
    // GroupBoxSREG3.Visible := True; // Có thể ẩn nếu Macronix
  end
  else if SPI_CMD_Type = SPI_CMD_25_NAND then
  begin
    // Hiển thị UI cho SPI NAND
    // Ẩn các GroupBox của NOR
    GroupBoxSREG1.Visible := False;
    GroupBoxSREG2.Visible := False;
    GroupBoxSREG3.Visible := False;
    // Ẩn ComboBoxSRType và các label liên quan nếu cần, vì nó chỉ cho NOR
    ComboBoxSRType.Visible := False; // Hoặc giữ lại nếu bạn muốn hiển thị giao diện NOR bên dưới nhưng ẩn các nhóm chính
    // Label1.Caption := 'NAND Registers'; // (Tùy chọn) Cập nhật label nếu cần

    // Hiện các GroupBox của NAND
    GroupBoxNANDSREG1.Visible := True;
    GroupBoxNANDSREG2.Visible := True;
    GroupBoxNANDSREG3.Visible := True;
    GroupBoxNANDSpecial.Visible := True; // Hiện nhóm đặc biệt nếu có
  end
  else
  begin
    // Nếu loại chip không hỗ trợ, ẩn tất cả
    GroupBoxSREG1.Visible := False;
    GroupBoxSREG2.Visible := False;
    GroupBoxSREG3.Visible := False;
    GroupBoxNANDSREG1.Visible := False;
    GroupBoxNANDSREG2.Visible := False;
    GroupBoxNANDSREG3.Visible := False;
    GroupBoxNANDSpecial.Visible := False;
    ComboBoxSRType.Visible := False; // Hoặc giữ lại, tùy bạn
  end;
end;

// --- Thêm sự kiện cho các nút NAND ---
procedure TsregeditForm.ButtonUnprotectNANDClick(Sender: TObject);
var
  SPI_CMD_Type: Integer;
begin
  SPI_CMD_Type := MainForm.ComboSPICMD.ItemIndex;

  if SPI_CMD_Type <> SPI_CMD_25_NAND then
  begin
    LogPrint('Unprotect function is only available for SPI NAND.');
    Exit;
  end;

  try
    if not OpenDevice() then exit;
    EnterProgMode25NAND(SetSPISpeed(0)); // Gọi hàm vào chế độ NAND

    // Gọi hàm gỡ bảo vệ từ spi25NAND.pas
    if UsbAsp25NAND_Unprotect() < 0 then
    begin
        LogPrint('Failed to unprotect SPI NAND chip.');
    end
    else
    begin
        LogPrint('SPI NAND chip unprotected successfully.');
        // Có thể cần đọc lại register để cập nhật UI
        ButtonReadSregClick(Sender); // Gọi lại chức năng đọc
    end;

  finally
    ExitProgMode25NAND; // Gọi hàm ra chế độ NAND
    AsProgrammer.Programmer.DevClose;
  end;
end;

procedure TsregeditForm.ButtonReadBBTableClick(Sender: TObject);
var
  SPI_CMD_Type: Integer;
  BBTable: array[0..79] of byte; // Kích thước bảng BB thường là 80 byte
  i: integer;
  BBTableStr: string;
begin
  SPI_CMD_Type := MainForm.ComboSPICMD.ItemIndex;

  if SPI_CMD_Type <> SPI_CMD_25_NAND then
  begin
    LogPrint('Read BB Table function is only available for SPI NAND.');
    Exit;
  end;

  try
    if not OpenDevice() then exit;
    EnterProgMode25NAND(SetSPISpeed(0)); // Gọi hàm vào chế độ NAND

    // Gọi hàm đọc bảng Bad Block từ spi25NAND.pas
    if UsbAsp25NAND_ReadBBTable(BBTable) < 0 then
    begin
        LogPrint('Failed to read SPI NAND Bad Block Table.');
        Exit;
    end;

    // Chuyển đổi mảng byte sang chuỗi để in ra log
    BBTableStr := 'BB Table: ';
    for i := 0 to High(BBTable) do
    begin
        BBTableStr := BBTableStr + IntToHex(BBTable[i], 2) + ' ';
        if (i + 1) mod 16 = 0 then BBTableStr := BBTableStr + #13#10; // Xuống dòng sau mỗi 16 byte
    end;
    LogPrint(BBTableStr);

  finally
    ExitProgMode25NAND; // Gọi hàm ra chế độ NAND
    AsProgrammer.Programmer.DevClose;
  end;
end;

end.
