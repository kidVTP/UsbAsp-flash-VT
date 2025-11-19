unit spi25nand;

{$mode objfpc}

interface

uses
  Classes, Forms, SysUtils, utilfunc;

const
  // Opcode SPI NAND
  SPI_NAND_CMD_READ_PAGE = $13; // Load page data into cache
  SPI_NAND_CMD_READ_CACHE = $03; // Read data from cache
  SPI_NAND_CMD_READ_CACHE_END = $04; // Read data from cache with address increment
  SPI_NAND_CMD_PROGRAM_LOAD = $02; // Load data into cache
  SPI_NAND_CMD_PROGRAM_EXEC = $10; // Execute program (cache to flash)
  SPI_NAND_CMD_READ_STATUS = $0F; // Read status register
  SPI_NAND_CMD_READ_ID = $9F; // Read ID
  SPI_NAND_CMD_WRITE_ENABLE = $06; // Write enable
  SPI_NAND_CMD_WRITE_DISABLE = $04; // Write disable
  SPI_NAND_CMD_CHIP_ERASE = $C7; // Chip erase (may require multiple commands)
  SPI_NAND_CMD_BLOCK_ERASE = $D8; // Block erase
  SPI_NAND_CMD_RESET = $FF; // Reset
  SPI_NAND_CMD_READ_BB = $A5; // Read bad block table (Winbond specific)
  SPI_NAND_CMD_GET_FEATURE = $0F; // Get feature (status, protection, etc.)
  SPI_NAND_CMD_SET_FEATURE = $1F; // Set feature (status, protection, etc.)

  // Status Register bits
  SPI_NAND_STAT_BUSY = 0; // Busy bit (1 = Busy)
  SPI_NAND_STAT_FAIL = 2; // Erase/Program Fail bit (1 = Fail)
  SPI_NAND_STAT_CACHE_READY = 5; // Cache Ready bit (1 = Ready) (Winbond specific)
  SPI_NAND_STAT_PROT = 6; // Write Protect bit (1 = Protected)

  // Feature Register Address
  SPI_NAND_FEATURE_STATUS = $C0;
  SPI_NAND_FEATURE_PROTECTION = $A0;
  SPI_NAND_FEATURE_CONFIG = $B0; // Includes ECC enable bit

  // Page and Block sizes (example for W25N01GV)
  SPI_NAND_PAGE_SIZE = 2048; // Standard page data size
  SPI_NAND_SPARE_SIZE = 64;  // Spare area size
  SPI_NAND_TOTAL_PAGE_SIZE = SPI_NAND_PAGE_SIZE + SPI_NAND_SPARE_SIZE; // 2112 bytes
  SPI_NAND_PAGES_PER_BLOCK = 64;
  SPI_NAND_BLOCK_SIZE = SPI_NAND_PAGES_PER_BLOCK * SPI_NAND_TOTAL_PAGE_SIZE; // 135168 bytes

type
  MEMORY_ID_NAND = record
    ID9FH: array[0..2] of byte; // Standard JEDEC ID
    // Other ID methods might be needed depending on the chip
  end;

// Hàm kiểm tra trạng thái Busy
function UsbAsp25NAND_Busy(): boolean;

// Vào/ra chế độ lập trình
function EnterProgMode25NAND(spiSpeed: integer): boolean; // SendAB có thể không cần thiết cho NAND
procedure ExitProgMode25NAND;

// Đọc ID chip
function UsbAsp25NAND_ReadID(var ID: MEMORY_ID_NAND): integer;

// Đọc một trang vào cache, sau đó đọc dữ liệu từ cache
function UsbAsp25NAND_ReadPage(PageAddr: longword; var buffer: array of byte; bufflen: integer): integer;
// Ghi dữ liệu vào cache, sau đó thực thi ghi vào flash
function UsbAsp25NAND_WritePage(PageAddr: longword; buffer: array of byte; bufflen: integer): integer;
// Xóa một block
function UsbAsp25NAND_EraseBlock(BlockAddr: longword): integer;
// Xóa toàn chip
function UsbAsp25NAND_ChipErase(): integer;

// Đọc thanh ghi trạng thái
function UsbAsp25NAND_ReadStatus(var sreg: byte; FeatureAddr: byte = SPI_NAND_FEATURE_STATUS): integer; 
// Ghi thanh ghi config trạng thái (thường thông qua Set Feature)
function UsbAsp25NAND_WriteConfigStatus(sreg: byte; FeatureAddr: byte): integer;

function UsbAsp25NAND_Unprotect(): integer; 
// Đọc bảng Bad Block (Winbond)
function UsbAsp25NAND_ReadBBTable(var BBTable: array of byte): integer;

// Giao tiếp cơ bản SPI (wrapper) - ĐỔI TÊN ĐỂ TRÁNH XUNG ĐỘT
function SPIReadNAND(CS: byte; BufferLen: integer; out buffer: array of byte): integer;
function SPIWriteNAND(CS: byte; BufferLen: integer; buffer: array of byte): integer;

// Hàm tiện ích
function UsbAsp25NAND_WaitWhileBusy(TimeoutMs: integer = 1000): boolean; // Chờ chip bớt bận

implementation

uses Main; // Giả sử AsProgrammer.Programmer được định nghĩa ở đây

// --- Giao tiếp cơ bản SPI - DÙNG TÊN MỚI ---
function SPIReadNAND(CS: byte; BufferLen: integer; out buffer: array of byte): integer;
begin
  result := AsProgrammer.Programmer.SPIRead(CS, BufferLen, buffer);
end;

function SPIWriteNAND(CS: byte; BufferLen: integer; buffer: array of byte): integer;
begin
  result := AsProgrammer.Programmer.SPIWrite(CS, BufferLen, buffer);
end;

// --- Hàm tiện ích ---
function UsbAsp25NAND_WaitWhileBusy(TimeoutMs: integer = 1000): boolean;
var
  sreg: byte;
  StartTime: QWord;
begin
  Result := False;
  StartTime := GetTickCount64;
  repeat
    if UsbAsp25NAND_ReadStatus(sreg) < 0 then Exit; // Lỗi đọc trạng thái
    if not IsBitSet(sreg, SPI_NAND_STAT_BUSY) then
    begin
      Result := True;
      Break;
    end;
    // Kiểm tra timeout
    if (GetTickCount64 - StartTime) > QWord(TimeoutMs) then Exit;
  until False;
end;

// --- Hàm chính ---

function UsbAsp25NAND_Busy: boolean;
var
  sreg: byte;
begin
  Result := True; // Mặc định là bận nếu không thể đọc
  if UsbAsp25NAND_ReadStatus(sreg) >= 0 then
  begin
    if not IsBitSet(sreg, SPI_NAND_STAT_BUSY) then Result := False;
  end;
  // LogPrint('Status Register: ' + IntToHex(sreg, 2) + ' Busy: ' + BoolToStr(Result, 'Yes', 'No'));
end;

function EnterProgMode25NAND(spiSpeed: integer): boolean;
begin
  result := AsProgrammer.Programmer.SPIInit(spiSpeed);
  if Result then
  begin
    sleep(50); // Chờ ổn định sau khi khởi tạo SPI

    // Gửi lệnh Reset để đảm bảo chip ở trạng thái rõ ràng
    // SỬA DÒNG NÀY: Gọi AsProgrammer.Programmer.SPIWrite trực tiếp với mảng inline
    if AsProgrammer.Programmer.SPIWrite(1, 1, [SPI_NAND_CMD_RESET]) < 0 then
    begin
      LogPrint('Error sending reset command');
      Result := False; // Thất bại nếu không gửi được reset
    end
    else
    begin
      sleep(1); // Chờ reset hoàn tất
      // Có thể cần chờ busy ở đây, nhưng thường rất nhanh
      if not UsbAsp25NAND_WaitWhileBusy(100) then // Chờ 100ms max cho reset
      begin
        LogPrint('Chip did not become ready after reset');
        Result := False;
      end;
    end;
  end;
  // LogPrint('EnterProgMode25NAND: ' + BoolToStr(Result, 'Success', 'Failed'));
end;

procedure ExitProgMode25NAND;
begin
  AsProgrammer.Programmer.SPIDeinit;
  // LogPrint('ExitProgMode25NAND called');
end;

function UsbAsp25NAND_ReadID(var ID: MEMORY_ID_NAND): integer;
var
  buffer: array[0..2] of byte; // Đọc 3 byte ID cơ bản
  cmd_buff: array[0..1] of byte;

begin
  Result := -1; // Mặc định là lỗi

  FillByte(buffer, 3, $FF); // Gửi dummy byte nếu cần
    // 1. Gửi lệnh Load Page vào Cache
  cmd_buff[0] := SPI_NAND_CMD_READ_ID;
  cmd_buff[1] := $00;
  // SỬA DÒNG NÀY: Gọi AsProgrammer.Programmer.SPIWrite trực tiếp với mảng inline
  if AsProgrammer.Programmer.SPIWrite(0, 2, cmd_buff) < 0 then Exit;

  if SPIReadNAND(1, 3, buffer) <> 3 then Exit;

  move(buffer, ID.ID9FH, 3);
  Result := 3; // Trả về số byte đã đọc
  
  LogPrint('SPI_NAND_TOTAL_PAGE_SIZE ' + IntToStr(SPI_NAND_TOTAL_PAGE_SIZE) + ' bytes');
  LogPrint('SPI_NAND_BLOCK_SIZE ' + IntToStr(SPI_NAND_BLOCK_SIZE) + ' bytes');
  //LogPrint('SPI NAND ID Read: ' + IntToHex(ID.ID9FH[0], 2) + ' ' + IntToHex(ID.ID9FH[1], 2) + ' ' + IntToHex(ID.ID9FH[2], 2));
end;

function UsbAsp25NAND_ReadPage(PageAddr: longword; var buffer: array of byte; bufflen: integer): integer;
var
  cmd_buff: array[0..3] of byte;
  addr_bytes: array[0..2] of byte;
  bytes_to_read: integer;
  sreg: byte;
begin
  Result := -1; // Mặc định là lỗi

  // Giới hạn số byte đọc theo kích thước buffer đầu vào
  bytes_to_read := bufflen;
  if bytes_to_read > SPI_NAND_TOTAL_PAGE_SIZE then
    bytes_to_read := SPI_NAND_TOTAL_PAGE_SIZE;

  // 1. Gửi lệnh Load Page vào Cache
  cmd_buff[0] := SPI_NAND_CMD_READ_PAGE;
  addr_bytes[0] := (PageAddr shr 16) and $FF;
  addr_bytes[1] := (PageAddr shr 8) and $FF;
  addr_bytes[2] := PageAddr and $FF;
  move(addr_bytes, cmd_buff[1], 3);

  // SỬA DÒNG NÀY: Gọi AsProgrammer.Programmer.SPIWrite trực tiếp với mảng cmd_buff
  if AsProgrammer.Programmer.SPIWrite(1, 4, cmd_buff) < 0 then Exit;

  // 2. Chờ chip load xong (trạng thái Busy = 0)
  if not UsbAsp25NAND_WaitWhileBusy() then
  begin
    LogPrint('Read Page: Timeout waiting for busy to clear after Load Page command');
    Exit;
  end;

  // 3. Đọc trạng thái sau khi load (kiểm tra lỗi)
  if UsbAsp25NAND_ReadStatus(sreg) < 0 then Exit;
  if IsBitSet(sreg, SPI_NAND_STAT_FAIL) then
  begin
    LogPrint('Read Page: Load Page failed (Fail bit set). Status: ' + IntToHex(sreg, 2));
    Exit; // Trả lỗi nếu có lỗi load
  end;

  // 4. Gửi lệnh Read từ Cache
  cmd_buff[0] := SPI_NAND_CMD_READ_CACHE;
  cmd_buff[1] := 0; // Address byte 1
  cmd_buff[2] := 0; // Address byte 2
  cmd_buff[3] := 0; // Address byte 3 (offset within page)

  // SỬA DÒNG NÀY: Gọi AsProgrammer.Programmer.SPIWrite trực tiếp với mảng cmd_buff
  if AsProgrammer.Programmer.SPIWrite(0, 4, cmd_buff) < 0 then Exit;

  // 5. Đọc dữ liệu từ cache
  if SPIReadNAND(1, bytes_to_read, buffer) <> bytes_to_read then Exit;

  Result := bytes_to_read; // Trả về số byte đã đọc thành công
  // LogPrint('Read Page ' + IntToStr(PageAddr) + ': ' + IntToStr(Result) + ' bytes');
end;

function UsbAsp25NAND_WritePage(PageAddr: longword; buffer: array of byte; bufflen: integer): integer;
var
  cmd_buff: array[0..3] of byte;
  addr_bytes: array[0..2] of byte;
  bytes_to_write: integer;
  sreg: byte;
begin
  Result := -1; // Mặc định là lỗi

  // Giới hạn số byte ghi theo kích thước buffer đầu vào và kích thước trang
  bytes_to_write := bufflen;
  if bytes_to_write > SPI_NAND_TOTAL_PAGE_SIZE then
    bytes_to_write := SPI_NAND_TOTAL_PAGE_SIZE;

  // 1. Gửi lệnh Write Enable
  // SỬA DÒNG NÀY: Gọi AsProgrammer.Programmer.SPIWrite trực tiếp với mảng inline
  if AsProgrammer.Programmer.SPIWrite(1, 1, [SPI_NAND_CMD_WRITE_ENABLE]) < 0 then Exit;

  // 2. Gửi lệnh Load Data vào Cache (Program Load)
  cmd_buff[0] := SPI_NAND_CMD_PROGRAM_LOAD;
  cmd_buff[1] := 0; // Address byte 1
  cmd_buff[2] := 0; // Address byte 2
  // Không có địa chỉ 3 cho lệnh load data này, dữ liệu sẽ theo sau

  // SỬA DÒNG NÀY: Gọi AsProgrammer.Programmer.SPIWrite trực tiếp với mảng cmd_buff
  if AsProgrammer.Programmer.SPIWrite(0, 3, cmd_buff) < 0 then Exit;
  // SỬA DÒNG NÀY: Gọi AsProgrammer.Programmer.SPIWrite trực tiếp với mảng buffer
  if AsProgrammer.Programmer.SPIWrite(1, bytes_to_write, buffer) < 0 then Exit;

  // 3. Gửi lệnh Execute Program (Cache to Flash)
  cmd_buff[0] := SPI_NAND_CMD_PROGRAM_EXEC;
  addr_bytes[0] := (PageAddr shr 16) and $FF;
  addr_bytes[1] := (PageAddr shr 8) and $FF;
  addr_bytes[2] := PageAddr and $FF;
  move(addr_bytes, cmd_buff[1], 3);

  // SỬA DÒNG NÀY: Gọi AsProgrammer.Programmer.SPIWrite trực tiếp với mảng cmd_buff
  if AsProgrammer.Programmer.SPIWrite(1, 4, cmd_buff) < 0 then Exit;

  // 4. Chờ chip ghi xong (trạng thái Busy = 0)
  if not UsbAsp25NAND_WaitWhileBusy(1000) then // Timeout 1000ms cho ghi
  begin
     LogPrint('Write Page: Timeout waiting for busy to clear after Program Exec command');
     Exit;
  end;

  // 5. Đọc trạng thái sau khi ghi (kiểm tra lỗi)
  if UsbAsp25NAND_ReadStatus(sreg) < 0 then Exit;
  if IsBitSet(sreg, SPI_NAND_STAT_FAIL) then
  begin
    LogPrint('Write Page: Program failed (Fail bit set). Status: ' + IntToHex(sreg, 2));
    Exit; // Trả lỗi nếu có lỗi ghi
  end;

  Result := bytes_to_write; // Trả về số byte đã ghi thành công
  // LogPrint('Write Page ' + IntToStr(PageAddr) + ': ' + IntToStr(Result) + ' bytes');
end;

function UsbAsp25NAND_EraseBlock(BlockAddr: longword): integer;
var
  cmd_buff: array[0..3] of byte;
  addr_bytes: array[0..2] of byte;
  sreg: byte;
begin
  Result := -1; // Mặc định là lỗi

  // 1. Gửi lệnh Write Enable
  // SỬA DÒNG NÀY: Gọi AsProgrammer.Programmer.SPIWrite trực tiếp với mảng inline
  if AsProgrammer.Programmer.SPIWrite(1, 1, [SPI_NAND_CMD_WRITE_ENABLE]) < 0 then Exit;

  // 2. Gửi lệnh Block Erase
  cmd_buff[0] := SPI_NAND_CMD_BLOCK_ERASE;
  addr_bytes[0] := (BlockAddr shr 16) and $FF; // Địa chỉ block là PageAddr của trang đầu tiên trong block
  addr_bytes[1] := (BlockAddr shr 8) and $FF;
  addr_bytes[2] := BlockAddr and $FF;
  move(addr_bytes, cmd_buff[1], 3);

  // SỬA DÒNG NÀY: Gọi AsProgrammer.Programmer.SPIWrite trực tiếp với mảng cmd_buff
  if AsProgrammer.Programmer.SPIWrite(1, 4, cmd_buff) < 0 then Exit;

  // 3. Chờ chip xóa xong (trạng thái Busy = 0)
  if not UsbAsp25NAND_WaitWhileBusy(10000) then // Timeout 10000ms cho xóa block
  begin
     LogPrint('Erase Block: Timeout waiting for busy to clear after Block Erase command');
     Exit;
  end;

  // 4. Đọc trạng thái sau khi xóa (kiểm tra lỗi)
  if UsbAsp25NAND_ReadStatus(sreg) < 0 then Exit;
  if IsBitSet(sreg, SPI_NAND_STAT_FAIL) then
  begin
    LogPrint('Erase Block: Erase failed (Fail bit set). Status: ' + IntToHex(sreg, 2));
    Exit; // Trả lỗi nếu có lỗi xóa
  end;

  Result := 0; // Trả về 0 để chỉ định thành công (không có byte nào được trả về)
  // LogPrint('Erase Block ' + IntToStr(BlockAddr) + ': Success');
end;

function UsbAsp25NAND_ChipErase(): integer;
var
  //sreg: byte;
  BlockAddr: longword;
  MaxBlocks: longword;
  i: longword;
begin
  Result := -1; // Mặc định là lỗi
  LogPrint('Chip Erase: Starting block-by-block erase for SPI NAND (no single command).');

  // Tính số block cần xóa (dựa trên _IC_Size nếu có, hoặc kích thước chip)
  // Giả sử _IC_Size là tổng dung lượng, _IC_Page là kích thước trang (2048)
  if (CurrentICParam.Size > 0) and (CurrentICParam.Page = SPI_NAND_PAGE_SIZE) then
  begin
    MaxBlocks := (CurrentICParam.Size div SPI_NAND_PAGE_SIZE) div SPI_NAND_PAGES_PER_BLOCK;
  end
  else
  begin
    // Fallback: giả sử chip 1GB (W25N01GV)
    MaxBlocks := (1024 * 1024 * 1024 div SPI_NAND_PAGE_SIZE) div SPI_NAND_PAGES_PER_BLOCK; // 1024 blocks
  end;

  LogPrint('Chip Erase: Calculated ' + IntToStr(MaxBlocks) + ' blocks to erase.');

  for i := 0 to MaxBlocks - 1 do
  begin
    BlockAddr := i * SPI_NAND_PAGES_PER_BLOCK; // Địa chỉ trang đầu tiên của block
    // LogPrint('Erasing Block ' + IntToStr(i) + ' (PageAddr: ' + IntToStr(BlockAddr) + ')...');
    if UsbAsp25NAND_EraseBlock(BlockAddr) < 0 then
    begin
      LogPrint('Chip Erase: Failed at Block ' + IntToStr(i) + '. Aborting.');
      Exit; // Thoát nếu một block không xóa được
    end;
    LogPrint('Block ' + IntToStr(i) + ' erased successfully.');
    // Cập nhật ProgressBar nếu cần
    // ProgressBar(1);
  end;

  LogPrint('Chip Erase: Completed all blocks.');
  Result := 0; // Thành công
end;

function UsbAsp25NAND_ReadStatus(var sreg: byte; FeatureAddr: byte = SPI_NAND_FEATURE_STATUS): integer; // Thêm tham số
var
  cmd_buff: array[0..1] of byte;
begin
  Result := -1; // Mặc định là lỗi

  cmd_buff[0] := SPI_NAND_CMD_GET_FEATURE;
  cmd_buff[1] := FeatureAddr; // Địa chỉ thanh ghi tính năng

  if AsProgrammer.Programmer.SPIWrite(0, 2, cmd_buff) < 0 then Exit;
  if AsProgrammer.Programmer.SPIRead(1, 1, sreg) <> 1 then Exit;

  Result := 0; // Thành công
  // LogPrint('Read Feature Register 0x' + IntToHex(FeatureAddr, 2) + ': ' + IntToHex(sreg, 2));
end;

function UsbAsp25NAND_WriteConfigStatus(sreg: byte; FeatureAddr: byte): integer; // Thêm hàm mới
var
  cmd_buff: array[0..2] of byte;
  sreg_read: byte;
begin
  Result := -1; // Mặc định là lỗi

  // 1. Gửi lệnh Write Enable
  if AsProgrammer.Programmer.SPIWrite(1, 1, [SPI_NAND_CMD_WRITE_ENABLE]) < 0 then Exit;

  // 2. Gửi lệnh Set Feature
  cmd_buff[0] := SPI_NAND_CMD_SET_FEATURE;
  cmd_buff[1] := FeatureAddr;
  cmd_buff[2] := sreg;

  if AsProgrammer.Programmer.SPIWrite(1, 3, cmd_buff) < 0 then Exit;

  // 3. Chờ xong (nếu cần, thường nhanh)
  // UsbAsp25NAND_WaitWhileBusy(100);

  // 4. Kiểm tra lại giá trị đã ghi
  if UsbAsp25NAND_ReadStatus(sreg_read, FeatureAddr) < 0 then Exit; // Đọc lại thanh ghi cụ thể
  if sreg_read <> sreg then
  begin
    LogPrint('Write Feature Register 0x' + IntToHex(FeatureAddr, 2) + ': Verification failed. Wrote: ' + IntToHex(sreg, 2) + ', Read: ' + IntToHex(sreg_read, 2));
    Exit; // Trả lỗi nếu xác minh thất bại
  end;

  Result := 0; // Thành công
  LogPrint('Write Feature Register 0x' + IntToHex(FeatureAddr, 2) + ': ' + IntToHex(sreg, 2));
end;

function UsbAsp25NAND_Unprotect(): integer; 
var
  current_prot_reg: byte;
  new_prot_reg: byte;
begin
  Result := -1; // Mặc định là lỗi

  // 1. Đọc thanh ghi bảo vệ hiện tại
  if UsbAsp25NAND_ReadStatus(current_prot_reg, SPI_NAND_FEATURE_PROTECTION) < 0 then
  begin
     LogPrint('Unprotect: Failed to read protection register.');
     Exit;
  end;

  LogPrint('Old Protection Reg: ' + IntToHex(current_prot_reg, 2));

  // 2. Xác định giá trị mới để gỡ bảo vệ (thường là 0x00)
  // Các bit bảo vệ (ví dụ: BP0-BP2, BP3-BP5) thường ở mức cao để kích hoạt
  // Ghi 0x00 sẽ tắt tất cả các bit bảo vệ
  new_prot_reg := $00;

  // 3. Ghi giá trị mới vào thanh ghi bảo vệ
  if UsbAsp25NAND_WriteConfigStatus(new_prot_reg, SPI_NAND_FEATURE_PROTECTION) < 0 then
  begin
     LogPrint('Unprotect: Failed to write protection register.');
     Exit;
  end;

  // 4. Đọc lại để xác nhận
  if UsbAsp25NAND_ReadStatus(current_prot_reg, SPI_NAND_FEATURE_PROTECTION) < 0 then
  begin
     LogPrint('Unprotect: Failed to re-read protection register for verification.');
     Exit; // Có thể vẫn muốn tiếp tục nếu ghi thành công
  end;

  LogPrint('New Protection Reg: ' + IntToHex(current_prot_reg, 2));

  if current_prot_reg = new_prot_reg then
  begin
    Result := 0; // Thành công
    LogPrint('Chip unprotected successfully.');
  end
  else
  begin
     LogPrint('Unprotect: Verification failed. Expected: ' + IntToHex(new_prot_reg, 2) + ', Read: ' + IntToHex(current_prot_reg, 2));
     // Tùy chọn: có thể trả lỗi hoặc ghi nhận cảnh báo
     // Exit; // Nếu muốn lỗi khi xác minh thất bại
  end;
end;

function UsbAsp25NAND_ReadBBTable(var BBTable: array of byte): integer;
var
  table_size: integer; // Kích thước bảng BB (phụ thuộc chip, script dùng 80)
begin
  Result := -1; // Mặc định là lỗi

  // Giả sử kích thước bảng BB là 80 byte như trong script
  table_size := 80;
  if High(BBTable) + 1 < table_size then
  begin
    LogPrint('ReadBBTable: Buffer too small');
    Exit;
  end;

  // 1. Gửi lệnh Read BB Table - SỬA DÒNG NÀY: Gọi AsProgrammer.Programmer.SPIWrite trực tiếp với mảng inline
  if AsProgrammer.Programmer.SPIWrite(0, 2, [SPI_NAND_CMD_READ_BB, 0]) < 0 then Exit; // Gửi opcode và 1 byte dummy

  // 2. Đọc dữ liệu bảng - SỬA DÒNG NÀY: Gọi AsProgrammer.Programmer.SPIRead trực tiếp với mảng BBTable
  if AsProgrammer.Programmer.SPIRead(1, table_size, BBTable) <> table_size then Exit;

  Result := table_size; // Trả về số byte đã đọc
  LogPrint('ReadBBTable: Read ' + IntToStr(Result) + ' bytes');
end;

end.
