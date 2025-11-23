unit spi25nand;

{$mode objfpc}

interface

uses
  Classes, Forms, SysUtils, utilfunc;

const
  // ============================================================================
  // MANUFACTURER IDs
  // ============================================================================
  MFG_ID_WINBOND     = $EF;  // Winbond W25N series
  MFG_ID_MICRON      = $2C;  // Micron MT29F series
  MFG_ID_GIGADEVICE  = $C8;  // GigaDevice GD5F series
  MFG_ID_MACRONIX    = $C2;  // Macronix MX35 series
  MFG_ID_TOSHIBA     = $98;  // Toshiba/Kioxia
  MFG_ID_ESMT        = $C8;  // ESMT (same as GigaDevice)
  MFG_ID_FORESEE     = $CD;  // Foresee/Longsys FS35ND series
  MFG_ID_XTX         = $0B;  // XTX Technology
  MFG_ID_DOSILICON   = $E5;  // Dosilicon
  MFG_ID_FMSH        = $A1;  // FM (Fudan Microelectronics)
  MFG_ID_HEYANG      = $C9;  // HeYang Tek
  MFG_ID_PARAGON     = $A1;  // Paragon
  MFG_ID_ATO         = $9B;  // ATO Solution
  MFG_ID_ZBIT        = $5E;  // Zbit Semiconductor

  // ============================================================================
  // SPI NAND OPCODES
  // ============================================================================
  SPI_NAND_CMD_READ_PAGE = $13;
  SPI_NAND_CMD_READ_CACHE = $03;
  SPI_NAND_CMD_READ_CACHE_FAST = $0B;
  SPI_NAND_CMD_READ_CACHE_X2 = $3B;
  SPI_NAND_CMD_READ_CACHE_X4 = $6B;
  SPI_NAND_CMD_READ_CACHE_DUAL_IO = $BB;
  SPI_NAND_CMD_READ_CACHE_QUAD_IO = $EB;
  SPI_NAND_CMD_PROGRAM_LOAD = $02;
  SPI_NAND_CMD_PROGRAM_LOAD_X4 = $32;
  SPI_NAND_CMD_PROGRAM_LOAD_RANDOM = $84;
  SPI_NAND_CMD_PROGRAM_EXEC = $10;
  SPI_NAND_CMD_READ_ID = $9F;
  SPI_NAND_CMD_WRITE_ENABLE = $06;
  SPI_NAND_CMD_WRITE_DISABLE = $04;
  SPI_NAND_CMD_BLOCK_ERASE = $D8;
  SPI_NAND_CMD_RESET = $FF;
  SPI_NAND_CMD_GET_FEATURE = $0F;
  SPI_NAND_CMD_SET_FEATURE = $1F;
  SPI_NAND_CMD_READ_BB = $A5;  // Winbond specific

  // Status Register bits
  SPI_NAND_STAT_BUSY = 0;
  SPI_NAND_STAT_WEL = 1;
  SPI_NAND_STAT_FAIL = 2;
  SPI_NAND_STAT_ECC0 = 4;
  SPI_NAND_STAT_ECC1 = 5;

  // Feature Register Address
  SPI_NAND_FEATURE_PROTECTION = $A0;
  SPI_NAND_FEATURE_CONFIG = $B0;
  SPI_NAND_FEATURE_STATUS = $C0;
  SPI_NAND_FEATURE_DIE_SELECT = $D0;  // Multi-die chips

type
  MEMORY_ID_NAND = record
    ID9FH: array[0..2] of byte;
  end;

  // ============================================================================
  // PLANE ADDRESSING MODE
  // ============================================================================
  // pamLinear:       1 plane, no special handling
  // pamColumnBit12:  2 planes, plane bit in Column Address bit 12 (Micron, GigaDevice)
  // pamRowBit16:     2 planes, plane bit in Row Address PA[16] (Winbond W25N02)
  // pamRowBit17_16:  4 planes, plane bits in Row Address PA[17:16] (Winbond W25N04)
  TPlaneAddressMode = (pamLinear, pamColumnBit12, pamRowBit16, pamRowBit17_16);

  // ============================================================================
  // CHIP CHARACTERISTICS - Để mở rộng dễ dàng
  // ============================================================================
  TManufacturerInfo = record
    ID: Byte;
    Name: string;
    IDLength: integer;
    HasSignature: boolean;
  end;

  TParsedChipID = record
    IsValid: Boolean;
    RawBytes: array[0..2] of Byte;
    IDString: string;
    MfgInfo: TManufacturerInfo;
  end;

  TSpiNandChipInfo = record
    RowAddrBytes: byte;      // 2 hoặc 3 bytes cho Row Address
    ColAddrBytes: byte;      // 2 bytes cho Column Address
    DummyBytes: byte;        // Dummy bytes sau Column Address (0 hoặc 1)
    PlaneMode: TPlaneAddressMode;
    HasInternalECC: boolean;
  end;

// Main functions
function UsbAsp25NAND_Busy(): boolean;
function EnterProgMode25NAND(spiSpeed: integer): boolean;
procedure ExitProgMode25NAND;
function UsbAsp25NAND_ReadID(var ID: MEMORY_ID_NAND): integer;
function UsbAsp25NAND_ReadPage(PageAddr: longword; var buffer: array of byte; bufflen: integer): integer;
function UsbAsp25NAND_WritePage(PageAddr: longword; buffer: array of byte; bufflen: integer; spi_nand_total_page_size: integer): integer;
function UsbAsp25NAND_EraseBlock(BlockAddr: longword): integer;
function UsbAsp25NAND_ChipErase(): integer;
function UsbAsp25NAND_ReadStatus(var sreg: byte; FeatureAddr: byte = SPI_NAND_FEATURE_STATUS): integer;
function UsbAsp25NAND_WriteConfigStatus(sreg: byte; FeatureAddr: byte): integer;
function UsbAsp25NAND_Unprotect(): integer;
function UsbAsp25NAND_ReadBBTable(var BBTable: array of byte): integer;

// Helper functions
function GetManufacturerInfo(MfgID: byte): TManufacturerInfo;
function ParseChipID(const ID_NAND: MEMORY_ID_NAND): TParsedChipID;
function FormatChipIDString(const ParsedID: TParsedChipID): string;
function GetManufacturerID(): byte;
function GetPlaneAddressMode(MfgID: byte; Planes: integer): TPlaneAddressMode;
function GetChipInfo(MfgID: byte; Planes: integer): TSpiNandChipInfo;
function UsbAsp25NAND_WaitWhileBusy(TimeoutMs: integer = 1000): boolean;

implementation

uses Main;

// ============================================================================
// MANUFACTURER DATABASE - Cơ sở dữ liệu nhà sản xuất
// ============================================================================
const
  MANUFACTURER_DATABASE: array[0..11] of TManufacturerInfo = (
    (ID: $2C; Name: 'Micron';      IDLength: 2; HasSignature: False),
    (ID: $C8; Name: 'GigaDevice';  IDLength: 2; HasSignature: False),
    (ID: $C2; Name: 'Macronix';    IDLength: 2; HasSignature: False),
    (ID: $E5; Name: 'Dosilicon';   IDLength: 2; HasSignature: False),
    (ID: $0B; Name: 'XTX';         IDLength: 2; HasSignature: False),
    (ID: $EF; Name: 'Winbond';     IDLength: 3; HasSignature: True),   // AA signature
    (ID: $98; Name: 'Kioxia';      IDLength: 3; HasSignature: False),
    (ID: $CD; Name: 'Foresee';     IDLength: 3; HasSignature: False),
    (ID: $A1; Name: 'FMSH';        IDLength: 2; HasSignature: False),
    (ID: $C9; Name: 'HeYang';      IDLength: 2; HasSignature: False),
    (ID: $5E; Name: 'Zbit';        IDLength: 2; HasSignature: False),
    (ID: $9B; Name: 'ATO';         IDLength: 2; HasSignature: False)
  );

// ============================================================================
// ID PARSING FUNCTIONS - Hàm xử lý ID
// ============================================================================

// Tìm thông tin manufacturer từ ID byte
function GetManufacturerInfo(MfgID: byte): TManufacturerInfo;
var
  i: integer;
begin
  // Tìm trong database
  for i := 0 to High(MANUFACTURER_DATABASE) do
  begin
    if MANUFACTURER_DATABASE[i].ID = MfgID then
    begin
      Result := MANUFACTURER_DATABASE[i];
      Exit;
    end;
  end;
  
  // Nếu không tìm thấy, trả về default (2 bytes, no signature)
  Result.ID := MfgID;
  Result.Name := 'Unknown';
  Result.IDLength := 2;  // Default: assume 2-byte ID
  Result.HasSignature := False;
end;

// Parse ID từ chip thành cấu trúc có thông tin đầy đủ
function ParseChipID(const ID_NAND: MEMORY_ID_NAND): TParsedChipID;
begin
  // Copy raw bytes
  Move(ID_NAND.ID9FH, Result.RawBytes, 3);
  
  // Lấy thông tin manufacturer từ byte đầu tiên
  Result.MfgInfo := GetManufacturerInfo(ID_NAND.ID9FH[0]);
  
  // Tạo ID string theo đúng độ dài
  case Result.MfgInfo.IDLength of
    2: Result.IDString := UpperCase(
         IntToHex(ID_NAND.ID9FH[0], 2) + 
         IntToHex(ID_NAND.ID9FH[1], 2));
    3: Result.IDString := UpperCase(
         IntToHex(ID_NAND.ID9FH[0], 2) + 
         IntToHex(ID_NAND.ID9FH[1], 2) + 
         IntToHex(ID_NAND.ID9FH[2], 2));
    else Result.IDString := UpperCase(
           IntToHex(ID_NAND.ID9FH[0], 2) + 
           IntToHex(ID_NAND.ID9FH[1], 2));
  end;
  
  // Kiểm tra tính hợp lệ
  Result.IsValid := (ID_NAND.ID9FH[0] <> $00) and (ID_NAND.ID9FH[0] <> $FF);
end;

// Format thông tin ID thành chuỗi log đẹp
function FormatChipIDString(const ParsedID: TParsedChipID): string;
begin
  Result := '';
  Result := Result + 'Raw ID Bytes: ' + 
            IntToHex(ParsedID.RawBytes[0], 2) + ' ' + 
            IntToHex(ParsedID.RawBytes[1], 2) + ' ' + 
            IntToHex(ParsedID.RawBytes[2], 2) + #13#10;
  Result := Result + 'Manufacturer: ' + ParsedID.MfgInfo.Name + 
            ' (0x' + IntToHex(ParsedID.MfgInfo.ID, 2) + ')' + #13#10;
  Result := Result + 'ID Length:    ' + IntToStr(ParsedID.MfgInfo.IDLength) + ' bytes' + #13#10;
  
  if ParsedID.MfgInfo.HasSignature then
    Result := Result + 'Signature:    0x' + IntToHex(ParsedID.RawBytes[1], 2) + 
              ' (Winbond specific)' + #13#10;
  
  Result := Result + 'Chip ID:      ' + ParsedID.IDString + #13#10;
  Result := Result + '----------------------------------------' + #13#10;
  
  // Format explanation
  case ParsedID.MfgInfo.IDLength of
    2: Result := Result + 'Format: [Manufacturer][Device]';
    3: begin
      if ParsedID.MfgInfo.HasSignature then
        Result := Result + 'Format: [Manufacturer][Signature][Device]'
      else
        Result := Result + 'Format: [Manufacturer][Device][Extra]';
    end;
  end;
end;

// ============================================================================
// MANUFACTURER DATABASE
// ============================================================================

{
  SPI NAND Manufacturer Support Matrix:
  
  ┌─────────────┬────────┬─────────────────────────────────────────────────────┐
  │ Manufacturer│ MfgID  │ Chips & Plane Addressing                            │
  ├─────────────┼────────┼─────────────────────────────────────────────────────┤
  │ Winbond     │ $EF    │ W25N01GV (1p), W25N02KV (2p-Row), W25N04KV (4p-Row)│
  │ Micron      │ $2C    │ MT29F1G, MT29F2G, MT29F4G (2p-Column bit12)        │
  │ GigaDevice  │ $C8    │ GD5F1GQ4, GD5F2GQ5 (2p-Column bit12)               │
  │ Macronix    │ $C2    │ MX35LF1G, MX35LF2G (1p/2p-Column bit12)            │
  │ Toshiba     │ $98    │ TC58CVG, TC58CYG series (Column bit12)             │
  │ Foresee     │ $CD    │ FS35ND01G, FS35ND02G, FS35ND04G (Linear/Column)    │
  │ XTX         │ $0B    │ XT26G01A, XT26G02A (Column bit12)                  │
  │ Dosilicon   │ $E5    │ DS35Q1GA, DS35Q2GA (Column bit12)                  │
  │ FMSH        │ $A1    │ FM25S01 (Linear)                                    │
  │ HeYang      │ $C9    │ HYF1GQ4, HYF2GQ4 (Column bit12)                    │
  │ Zbit        │ $5E    │ ZB35Q01A (Linear)                                   │
  │ ATO         │ $9B    │ ATO25D1GA (Linear)                                  │
  └─────────────┴────────┴─────────────────────────────────────────────────────┘
  
  Note: Hầu hết các chip 2-plane dùng Column bit 12 cho plane select,
        chỉ Winbond dùng Row Address PA[16] hoặc PA[17:16]
}

// Lấy Manufacturer ID từ tên chip hoặc ID đã đọc
function GetManufacturerID(): byte;
begin
  // Ưu tiên: parse từ tên chip
  if Pos('W25N', UpperCase(CurrentICParam.Name)) > 0 then
    Result := MFG_ID_WINBOND
  else if Pos('MT29F', UpperCase(CurrentICParam.Name)) > 0 then
    Result := MFG_ID_MICRON
  else if Pos('GD5F', UpperCase(CurrentICParam.Name)) > 0 then
    Result := MFG_ID_GIGADEVICE
  else if Pos('MX35', UpperCase(CurrentICParam.Name)) > 0 then
    Result := MFG_ID_MACRONIX
  else if Pos('TC58', UpperCase(CurrentICParam.Name)) > 0 then
    Result := MFG_ID_TOSHIBA
  else if Pos('FS35', UpperCase(CurrentICParam.Name)) > 0 then
    Result := MFG_ID_FORESEE
  else if Pos('XT26', UpperCase(CurrentICParam.Name)) > 0 then
    Result := MFG_ID_XTX
  else if Pos('DS35', UpperCase(CurrentICParam.Name)) > 0 then
    Result := MFG_ID_DOSILICON
  else if Pos('FM25', UpperCase(CurrentICParam.Name)) > 0 then
    Result := MFG_ID_FMSH
  else if Pos('HYF', UpperCase(CurrentICParam.Name)) > 0 then
    Result := MFG_ID_HEYANG
  else if Pos('ZB35', UpperCase(CurrentICParam.Name)) > 0 then
    Result := MFG_ID_ZBIT
  else if Pos('ATO25', UpperCase(CurrentICParam.Name)) > 0 then
    Result := MFG_ID_ATO
  else
    Result := MFG_ID_WINBOND;  // Default
end;

// Xác định chế độ địa chỉ plane dựa trên manufacturer và số planes
function GetPlaneAddressMode(MfgID: byte; Planes: integer): TPlaneAddressMode;
begin
  case Planes of
    1: Result := pamLinear;
    
    2: begin
      // Chỉ Winbond dùng Row bit cho plane select
      if MfgID = MFG_ID_WINBOND then
        Result := pamRowBit16      // W25N02KV: PA[16]
      else
        Result := pamColumnBit12;  // Tất cả hãng khác: Column bit 12
    end;
    
    4: begin
      if MfgID = MFG_ID_WINBOND then
        Result := pamRowBit17_16   // W25N04KV: PA[17:16]
      else
        Result := pamColumnBit12;  // Các hãng khác (nếu có 4-plane)
    end;
    
    else Result := pamLinear;
  end;
end;

// Lấy thông tin đặc tính chip theo manufacturer
function GetChipInfo(MfgID: byte; Planes: integer): TSpiNandChipInfo;
begin
  // Mặc định cho hầu hết chip
  Result.RowAddrBytes := 3;
  Result.ColAddrBytes := 2;
  Result.DummyBytes := 1;        // Hầu hết cần 1 dummy byte sau Column Address
  Result.PlaneMode := GetPlaneAddressMode(MfgID, Planes);
  Result.HasInternalECC := True;
  
  // Điều chỉnh theo manufacturer cụ thể
  case MfgID of
    MFG_ID_WINBOND: begin
      // Winbond: standard
      Result.DummyBytes := 1;
    end;
    
    MFG_ID_MICRON: begin
      // Micron: có thể có 8 dummy cycles = 1 byte
      Result.DummyBytes := 1;
    end;
    
    MFG_ID_FORESEE: begin
      // Foresee: 8 dummy clocks = 1 byte (như trong datasheet)
      Result.DummyBytes := 1;
    end;
    
    MFG_ID_GIGADEVICE: begin
      Result.DummyBytes := 1;
    end;
    
    MFG_ID_MACRONIX: begin
      Result.DummyBytes := 1;
    end;
    
    MFG_ID_TOSHIBA: begin
      Result.DummyBytes := 1;
    end;
    
    MFG_ID_XTX: begin
      Result.DummyBytes := 1;
    end;
  end;
end;

// ============================================================================
// ADDRESS CALCULATION FUNCTIONS
// ============================================================================

// Row Address cho Page Read (13h), Program Execute (10h), Block Erase (D8h)
procedure GetRowAddressBytes(PageAddr: longword; Planes: integer;
  PagesPerBlock: integer; MfgID: byte; out addr_bytes: array of byte);
var
  BlockNum, PageInBlock, BlockInPlane: longword;
  PlaneSelect: byte;
  RowAddr: longword;
  PlaneMode: TPlaneAddressMode;
begin
  PlaneMode := GetPlaneAddressMode(MfgID, Planes);
  
  case PlaneMode of
    pamRowBit17_16: begin
      // Winbond W25N04KV: PA[17:16] = Plane (2 bits)
      BlockNum := PageAddr div PagesPerBlock;
      PageInBlock := PageAddr mod PagesPerBlock;
      PlaneSelect := BlockNum and $03;
      BlockInPlane := BlockNum shr 2;
      RowAddr := (longword(PlaneSelect) shl 16) or (BlockInPlane shl 6) or PageInBlock;
      
      addr_bytes[0] := (RowAddr shr 16) and $FF;
      addr_bytes[1] := (RowAddr shr 8) and $FF;
      addr_bytes[2] := RowAddr and $FF;
    end;
    
    pamRowBit16: begin
      // Winbond W25N02KV: PA[16] = Plane (1 bit)
      BlockNum := PageAddr div PagesPerBlock;
      PageInBlock := PageAddr mod PagesPerBlock;
      PlaneSelect := BlockNum and $01;
      BlockInPlane := BlockNum shr 1;
      RowAddr := (longword(PlaneSelect) shl 16) or (BlockInPlane shl 6) or PageInBlock;
      
      addr_bytes[0] := (RowAddr shr 16) and $FF;
      addr_bytes[1] := (RowAddr shr 8) and $FF;
      addr_bytes[2] := RowAddr and $FF;
    end;
    
    else begin
      // pamLinear, pamColumnBit12: Row Address là linear 24-bit
      // Áp dụng cho: W25N01GV, MT29F, GD5F, FS35ND, MX35, TC58, XT26, v.v.
      addr_bytes[0] := (PageAddr shr 16) and $FF;
      addr_bytes[1] := (PageAddr shr 8) and $FF;
      addr_bytes[2] := PageAddr and $FF;
    end;
  end;
end;

// Column Address cho Program Load (02h), Read from Cache (03h)
function GetColumnAddress(PageAddr: longword; Planes: integer;
  PagesPerBlock: integer; MfgID: byte): word;
var
  BlockNum: longword;
  PlaneSelect: byte;
  PlaneMode: TPlaneAddressMode;
begin
  PlaneMode := GetPlaneAddressMode(MfgID, Planes);
  
  case PlaneMode of
    pamColumnBit12: begin
      // Micron, GigaDevice, Macronix, Toshiba, Foresee, XTX, etc.
      // Column bit 12 = Plane Select
      BlockNum := PageAddr div PagesPerBlock;
      PlaneSelect := BlockNum and $01;
      Result := word(PlaneSelect) shl 12;
    end;
    
    else begin
      // pamLinear: Không có plane
      // pamRowBit16, pamRowBit17_16: Plane đã ở Row Address (Winbond)
      Result := 0;
    end;
  end;
end;

// Row Address cho Block Erase
procedure GetBlockEraseRowAddress(BlockIndex: longword; Planes: integer;
  PagesPerBlock: integer; MfgID: byte; out addr_bytes: array of byte);
var
  PlaneSelect: byte;
  BlockInPlane: longword;
  RowAddr: longword;
  PageAddr: longword;
  PlaneMode: TPlaneAddressMode;
begin
  PlaneMode := GetPlaneAddressMode(MfgID, Planes);
  
  case PlaneMode of
    pamRowBit17_16: begin
      PlaneSelect := BlockIndex and $03;
      BlockInPlane := BlockIndex shr 2;
      RowAddr := (longword(PlaneSelect) shl 16) or (BlockInPlane shl 6);
      
      addr_bytes[0] := (RowAddr shr 16) and $FF;
      addr_bytes[1] := (RowAddr shr 8) and $FF;
      addr_bytes[2] := RowAddr and $FF;
    end;
    
    pamRowBit16: begin
      PlaneSelect := BlockIndex and $01;
      BlockInPlane := BlockIndex shr 1;
      RowAddr := (longword(PlaneSelect) shl 16) or (BlockInPlane shl 6);
      
      addr_bytes[0] := (RowAddr shr 16) and $FF;
      addr_bytes[1] := (RowAddr shr 8) and $FF;
      addr_bytes[2] := RowAddr and $FF;
    end;
    
    else begin
      PageAddr := BlockIndex * PagesPerBlock;
      addr_bytes[0] := (PageAddr shr 16) and $FF;
      addr_bytes[1] := (PageAddr shr 8) and $FF;
      addr_bytes[2] := PageAddr and $FF;
    end;
  end;
end;

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

function UsbAsp25NAND_WaitWhileBusy(TimeoutMs: integer = 1000): boolean;
var
  sreg: byte;
  StartTime: QWord;
begin
  Result := False;
  StartTime := GetTickCount64;
  repeat
    if UsbAsp25NAND_ReadStatus(sreg) < 0 then Exit;
    if not IsBitSet(sreg, SPI_NAND_STAT_BUSY) then
    begin
      Result := True;
      Break;
    end;
    if (GetTickCount64 - StartTime) > QWord(TimeoutMs) then Exit;
  until False;
end;

function SPIReadNAND(CS: byte; BufferLen: integer; out buffer: array of byte): integer;
begin
  result := AsProgrammer.Programmer.SPIRead(CS, BufferLen, buffer);
end;

function SPIWriteNAND(CS: byte; BufferLen: integer; buffer: array of byte): integer;
begin
  result := AsProgrammer.Programmer.SPIWrite(CS, BufferLen, buffer);
end;

// ============================================================================
// MAIN FUNCTIONS
// ============================================================================

function UsbAsp25NAND_Busy: boolean;
var
  sreg: byte;
begin
  Result := True;
  if UsbAsp25NAND_ReadStatus(sreg) >= 0 then
    if not IsBitSet(sreg, SPI_NAND_STAT_BUSY) then 
      Result := False;
end;

function EnterProgMode25NAND(spiSpeed: integer): boolean;
begin
  result := AsProgrammer.Programmer.SPIInit(spiSpeed);
  if Result then
  begin
    sleep(50);
    if AsProgrammer.Programmer.SPIWrite(1, 1, [SPI_NAND_CMD_RESET]) < 0 then
    begin
      LogPrint('Error sending reset command');
      Result := False;
    end
    else
    begin
      sleep(1);
      if not UsbAsp25NAND_WaitWhileBusy(100) then
      begin
        LogPrint('Chip not ready after reset');
        Result := False;
      end;
    end;
  end;
end;

procedure ExitProgMode25NAND;
begin
  AsProgrammer.Programmer.SPIDeinit;
end;

function UsbAsp25NAND_ReadID(var ID: MEMORY_ID_NAND): integer;
var
  buffer: array[0..2] of byte;
  cmd_buff: array[0..1] of byte;
begin
  Result := -1;
  FillByte(buffer, 3, $FF);
  cmd_buff[0] := SPI_NAND_CMD_READ_ID;
  cmd_buff[1] := $00;
  if AsProgrammer.Programmer.SPIWrite(0, 2, cmd_buff) < 0 then Exit;
  if SPIReadNAND(1, 3, buffer) <> 3 then Exit;
  move(buffer, ID.ID9FH, 3);
  Result := 3;
end;

// ============================================================================
// READ PAGE - Universal for all manufacturers
// ============================================================================
function UsbAsp25NAND_ReadPage(PageAddr: longword; var buffer: array of byte;
  bufflen: integer): integer;
var
  cmd_buff: array[0..4] of byte;  // Max: opcode + 2 col + 1 dummy = 4 bytes
  addr_bytes: array[0..2] of byte;
  bytes_to_read: integer;
  sreg: byte;
  column_addr: word;
  MfgID: byte;
  ChipInfo: TSpiNandChipInfo;
  cmd_len: integer;
begin
  Result := -1;
  bytes_to_read := bufflen;
  MfgID := GetManufacturerID();
  ChipInfo := GetChipInfo(MfgID, CurrentICParam.Planes);

  // 1. Page Read to Cache (13h) + Row Address (3 bytes)
  cmd_buff[0] := SPI_NAND_CMD_READ_PAGE;
  GetRowAddressBytes(PageAddr, CurrentICParam.Planes,
                     CurrentICParam.PagesPerBlock, MfgID, addr_bytes);
  move(addr_bytes, cmd_buff[1], 3);
  if AsProgrammer.Programmer.SPIWrite(1, 4, cmd_buff) < 0 then Exit;

  // 2. Wait
  if not UsbAsp25NAND_WaitWhileBusy() then
  begin
    LogPrint('Read Page ' + IntToStr(PageAddr) + ': Timeout');
    Exit;
  end;

  // 3. Check status
  if UsbAsp25NAND_ReadStatus(sreg) < 0 then Exit;

  // 4. Read from Cache (03h) + Column Address + Dummy
  cmd_buff[0] := SPI_NAND_CMD_READ_CACHE;
  column_addr := GetColumnAddress(PageAddr, CurrentICParam.Planes,
                                  CurrentICParam.PagesPerBlock, MfgID);
  cmd_buff[1] := (column_addr shr 8) and $FF;
  cmd_buff[2] := column_addr and $FF;
  
  // Thêm dummy byte(s) theo chip
  cmd_len := 3 + ChipInfo.DummyBytes;
  if ChipInfo.DummyBytes > 0 then
    cmd_buff[3] := $00;

  if AsProgrammer.Programmer.SPIWrite(0, cmd_len, cmd_buff) < 0 then Exit;
  if SPIReadNAND(1, bytes_to_read, buffer) <> bytes_to_read then Exit;

  Result := bytes_to_read;
end;

// ============================================================================
// WRITE PAGE - Universal for all manufacturers
// ============================================================================
function UsbAsp25NAND_WritePage(PageAddr: longword; buffer: array of byte;
  bufflen: integer; spi_nand_total_page_size: integer): integer;
var
  cmd_buff: array[0..3] of byte;
  addr_bytes: array[0..2] of byte;
  bytes_to_write: integer;
  sreg: byte;
  column_addr: word;
  MfgID: byte;
begin
  Result := -1;
  bytes_to_write := bufflen;
  if bytes_to_write > spi_nand_total_page_size then
    bytes_to_write := spi_nand_total_page_size;
  MfgID := GetManufacturerID();

  // 1. Write Enable
  if AsProgrammer.Programmer.SPIWrite(1, 1, [SPI_NAND_CMD_WRITE_ENABLE]) < 0 then Exit;

  // 2. Program Load (02h) + Column Address (2 bytes)
  cmd_buff[0] := SPI_NAND_CMD_PROGRAM_LOAD;
  column_addr := GetColumnAddress(PageAddr, CurrentICParam.Planes,
                                  CurrentICParam.PagesPerBlock, MfgID);
  cmd_buff[1] := (column_addr shr 8) and $FF;
  cmd_buff[2] := column_addr and $FF;

  if AsProgrammer.Programmer.SPIWrite(0, 3, cmd_buff) < 0 then Exit;
  if AsProgrammer.Programmer.SPIWrite(1, bytes_to_write, buffer) < 0 then Exit;

  // 3. Program Execute (10h) + Row Address (3 bytes)
  cmd_buff[0] := SPI_NAND_CMD_PROGRAM_EXEC;
  GetRowAddressBytes(PageAddr, CurrentICParam.Planes,
                     CurrentICParam.PagesPerBlock, MfgID, addr_bytes);
  move(addr_bytes, cmd_buff[1], 3);
  if AsProgrammer.Programmer.SPIWrite(1, 4, cmd_buff) < 0 then Exit;

  // 4. Wait and check
  if not UsbAsp25NAND_WaitWhileBusy(1000) then
  begin
    LogPrint('Write Page ' + IntToStr(PageAddr) + ': Timeout');
    Exit;
  end;

  if UsbAsp25NAND_ReadStatus(sreg) < 0 then Exit;
  if IsBitSet(sreg, SPI_NAND_STAT_FAIL) then
  begin
    LogPrint('Write Page ' + IntToStr(PageAddr) + ': Failed');
    Exit;
  end;

  Result := bytes_to_write;
end;

// ============================================================================
// ERASE BLOCK - Universal for all manufacturers
// ============================================================================
function UsbAsp25NAND_EraseBlock(BlockAddr: longword): integer;
var
  cmd_buff: array[0..3] of byte;
  addr_bytes: array[0..2] of byte;
  sreg: byte;
  BlockIndex: longword;
  MfgID: byte;
begin
  Result := -1;
  MfgID := GetManufacturerID();

  if CurrentICParam.PagesPerBlock > 0 then
    BlockIndex := BlockAddr div CurrentICParam.PagesPerBlock
  else
    BlockIndex := BlockAddr div 64;

  // 1. Write Enable
  if AsProgrammer.Programmer.SPIWrite(1, 1, [SPI_NAND_CMD_WRITE_ENABLE]) < 0 then Exit;

  // 2. Block Erase (D8h) + Row Address
  cmd_buff[0] := SPI_NAND_CMD_BLOCK_ERASE;
  GetBlockEraseRowAddress(BlockIndex, CurrentICParam.Planes,
                          CurrentICParam.PagesPerBlock, MfgID, addr_bytes);
  move(addr_bytes, cmd_buff[1], 3);
  if AsProgrammer.Programmer.SPIWrite(1, 4, cmd_buff) < 0 then Exit;

  // 3. Wait
  if not UsbAsp25NAND_WaitWhileBusy(10000) then
  begin
    LogPrint('Erase Block ' + IntToStr(BlockIndex) + ': Timeout');
    Exit;
  end;

  // 4. Check status
  if UsbAsp25NAND_ReadStatus(sreg) < 0 then Exit;
  if IsBitSet(sreg, SPI_NAND_STAT_FAIL) then
  begin
    LogPrint('Erase Block ' + IntToStr(BlockIndex) + ': Failed');
    Exit;
  end;

  Result := 0;
end;

// ============================================================================
// CHIP ERASE (Block by block)
// ============================================================================
function UsbAsp25NAND_ChipErase(): integer;
var
  BlockAddr: longword;
  MaxBlocks: longword;
  i: longword;
begin
  Result := -1;
  LogPrint('Chip Erase: Starting...');

  if (CurrentICParam.Size > 0) and (CurrentICParam.Page > 0) then
  begin
    if CurrentICParam.PagesPerBlock > 0 then
      MaxBlocks := (CurrentICParam.Size div CurrentICParam.Page) div CurrentICParam.PagesPerBlock
    else
      MaxBlocks := (CurrentICParam.Size div CurrentICParam.Page) div 64;
  end
  else
  begin
    LogPrint('Error: Chip parameters not defined');
    Exit;
  end;

  LogPrint('Total blocks: ' + IntToStr(MaxBlocks));

  for i := 0 to MaxBlocks - 1 do
  begin
    BlockAddr := i * CurrentICParam.PagesPerBlock;
    if UsbAsp25NAND_EraseBlock(BlockAddr) < 0 then
    begin
      LogPrint('Chip Erase: Failed at block ' + IntToStr(i));
      Exit;
    end;
    if (i mod 100) = 0 then
      LogPrint('Progress: ' + IntToStr(i) + '/' + IntToStr(MaxBlocks));
  end;

  LogPrint('Chip Erase: Complete');
  Result := 0;
end;

// ============================================================================
// STATUS AND CONFIGURATION
// ============================================================================

function UsbAsp25NAND_ReadStatus(var sreg: byte; FeatureAddr: byte = SPI_NAND_FEATURE_STATUS): integer;
var
  cmd_buff: array[0..1] of byte;
begin
  Result := -1;
  cmd_buff[0] := SPI_NAND_CMD_GET_FEATURE;
  cmd_buff[1] := FeatureAddr;
  if AsProgrammer.Programmer.SPIWrite(0, 2, cmd_buff) < 0 then Exit;
  if AsProgrammer.Programmer.SPIRead(1, 1, sreg) <> 1 then Exit;
  Result := 0;
end;

function UsbAsp25NAND_WriteConfigStatus(sreg: byte; FeatureAddr: byte): integer;
var
  cmd_buff: array[0..2] of byte;
  sreg_read: byte;
begin
  Result := -1;
  if AsProgrammer.Programmer.SPIWrite(1, 1, [SPI_NAND_CMD_WRITE_ENABLE]) < 0 then Exit;
  
  cmd_buff[0] := SPI_NAND_CMD_SET_FEATURE;
  cmd_buff[1] := FeatureAddr;
  cmd_buff[2] := sreg;
  if AsProgrammer.Programmer.SPIWrite(1, 3, cmd_buff) < 0 then Exit;
  
  sleep(1);
  
  if UsbAsp25NAND_ReadStatus(sreg_read, FeatureAddr) < 0 then Exit;
  if sreg_read <> sreg then
  begin
    LogPrint('Write Feature 0x' + IntToHex(FeatureAddr, 2) + ': Verify failed');
    Exit;
  end;
  
  Result := 0;
  LogPrint('Write Feature 0x' + IntToHex(FeatureAddr, 2) + ': 0x' + IntToHex(sreg, 2));
end;

function UsbAsp25NAND_Unprotect(): integer;
var
  current_prot_reg, new_prot_reg: byte;
begin
  Result := -1;
  
  if UsbAsp25NAND_ReadStatus(current_prot_reg, SPI_NAND_FEATURE_PROTECTION) < 0 then
  begin
    LogPrint('Unprotect: Failed to read protection register');
    Exit;
  end;
  LogPrint('Old Protection: 0x' + IntToHex(current_prot_reg, 2));
  
  new_prot_reg := $00;
  if UsbAsp25NAND_WriteConfigStatus(new_prot_reg, SPI_NAND_FEATURE_PROTECTION) < 0 then
  begin
    LogPrint('Unprotect: Failed to write protection register');
    Exit;
  end;
  
  if UsbAsp25NAND_ReadStatus(current_prot_reg, SPI_NAND_FEATURE_PROTECTION) < 0 then Exit;
  LogPrint('New Protection: 0x' + IntToHex(current_prot_reg, 2));
  
  if current_prot_reg = new_prot_reg then
  begin
    Result := 0;
    LogPrint('Chip unprotected successfully');
  end
  else
    LogPrint('Unprotect: Verification failed');
end;

function UsbAsp25NAND_ReadBBTable(var BBTable: array of byte): integer;
var
  table_size: integer;
begin
  Result := -1;
  table_size := 80;
  
  if High(BBTable) + 1 < table_size then
  begin
    LogPrint('ReadBBTable: Buffer too small');
    Exit;
  end;
  
  // Note: This command is Winbond specific (A5h)
  if AsProgrammer.Programmer.SPIWrite(0, 2, [SPI_NAND_CMD_READ_BB, 0]) < 0 then Exit;
  if AsProgrammer.Programmer.SPIRead(1, table_size, BBTable) <> table_size then Exit;
  
  Result := table_size;
  LogPrint('ReadBBTable: Read ' + IntToStr(Result) + ' bytes');
end;

end.
