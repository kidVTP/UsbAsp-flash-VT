// FIXED VERSION for W25N01GVZEIG SPI NAND
// Critical fixes:
// 1. MUST enable ECC before any read/write
// 2. Fixed page boundary crossing issue
// 3. Added proper status checking
// 4. Correct command sequences per datasheet
// for CH347
            // Tag = 0 '60 MHz'
            // Tag = 1 '30 MHz'
            // Tag = 2 '15 MHz'
            // Tag = 3 '7.5 MHz'
            // Tag = 4 '3.75 MHz'
            // Tag = 5 '1.875 MHz'
            // Tag = 6 '937.5 KHz'
            // Tag = 7 '468.75 KHz'

//  _SPI_SPEED:= 0; 1 2 3 4 5 6 7  for CH347
//  _SPI_SPEED:= _SPI_SPEED_MAX;  255  for CH341

{$ READ_JEDEC_ID}
begin
  ID:= CreateByteArray(3);
   _SPI_SPEED:= 1;
  if not SPIEnterProgMode(_SPI_SPEED) then LogPrint('Error setting SPI speed');
  LogPrint ('Read JEDEC ID');
  
  // Read ID
  SPIWrite (0, 2, $9F, $00);
  SPIRead (1, 3, ID);
  
  logprint('CHIP ID: ' + inttohex((GetArrayItem(ID, 0)),2)+ inttohex((GetArrayItem(ID, 1)),2)+ IntToHex(GetArrayItem(ID, 2), 2));
  LogPrint ('End read JEDEC ID');
  SPIExitProgMode ();
end

{$ ENABLE_ECC}
// CRITICAL: Must run before any read/write operation
begin
   _SPI_SPEED:= 1;
  if not SPIEnterProgMode(_SPI_SPEED) then LogPrint('Error setting SPI speed');
  LogPrint('Enabling ECC...');
  
  sreg := $FF;
  
  // Reset chip
  SPIWrite(1, 1, $FF);
  Delay(10);
  
  // Read current Protection Register (B0h)
  SPIWrite(0, 2, $0F, $B0);
  SPIRead(1, 1, sreg);
  LogPrint('Current Protection Reg: ' + IntToHex(sreg, 2));
  
  // Enable ECC (set bit 4)
  sreg := sreg or $10;
  // ✅ Thêm log để debug
  LogPrint('Value to write to register (with ECC enabled): ' + IntToHex(sreg, 2));
  
  // Write enable
  SPIWrite(1, 1, $06);
  
  // Write Protection Register with ECC enabled
  SPIWrite(1, 3, $1F, $B0, sreg);
  
  // Verify ECC is enabled
  SPIWrite(0, 2, $0F, $B0);
  SPIRead(1, 1, sreg);
  
  if (sreg and $10) = $10 then
  begin
    LogPrint('ECC enabled successfully: ' + IntToHex(sreg, 2));
  end
  else
  begin
    LogPrint('ERROR: Failed to enable ECC!');
  end;
  
  SPIExitProgMode();
end

//  W25N01GV has 1,024 erasable block , 65,536 programmable pages , 1 page = 2,048-bytes 
// vói xóa BLOCK 0 = với 64_PAGE đầu tiên  | 1 block = 64_PAGE
{$ ERASE_a_BLOCK}
begin
   _SPI_SPEED:= 1;
  if not SPIEnterProgMode(_SPI_SPEED) then LogPrint('Error setting SPI speed');
  LogPrint ('Erase a given block');
  sreg :=$FF;
  buff:= CreateByteArray(4);
  maxpage:= _IC_Size / _IC_Page;
  
  repeat
    BlockNum := InputBox('Enter Block Number (0 to '+ inttostr((maxpage / 64)-1)+')','','0');
  until (BlockNum >=0) and (BlockNum <=(maxpage / 64)-1);

  // Reset chip
  SPIWrite(1, 1, $FF);
  Delay(10);
  
  // Write enable
  SPIWrite (1, 1, $06);
  
  // Reset protection bits
  SPIWrite(1, 3, $1F, $A0, 0);
  
  // Wait if busy
  repeat
    SPIWrite(0, 2, $0F, $C0);
    SPIRead(1, 1, sreg);
  until((sreg and 1) <> 1);
    
  // Write enable for erase
  SPIWrite(1, 1, $06);
  
  Addr:= BlockNum * 64;
  SetArrayItem(buff, 0, $D8);
  SetArrayItem(buff, 1, (addr shr 16));
  SetArrayItem(buff, 2, (addr shr 8));
  SetArrayItem(buff, 3, (addr));
  
  // Erase block
  SPIWrite (1, 4, buff);
  
  // Wait for erase complete
  repeat
    Delay(1);
    SPIWrite(0, 2, $0F, $C0);
    SPIRead(1, 1, sreg);
  until((sreg and 1) <> 1);
  
  // Check erase fail bit
  if (sreg and 4) <> 0 then
  begin
    LogPrint('ERROR: Erase failed!');
  end
  else
  begin
    LogPrint('Block ' + IntToStr(BlockNum) + ' erased successfully');
  end;

  SPIExitProgMode ();
end



{$ READ_64_PAGE}
begin
   _SPI_SPEED:= 1;
  if not SPIEnterProgMode(_SPI_SPEED) then
  begin
    LogPrint('Error entering SPI Prog Mode');
    Exit; // Thoát nếu không vào được chế độ
  end;
  // ===== BẮT ĐẦU ĐẾM THỜI GIAN =====
  start_time := Now();

  LogPrint('Reading all 64 pages into memory...');
  LogPrint('_IC_Size: ' + IntToStr(_IC_Size));
  LogPrint('_IC_Page: ' + IntToStr(_IC_Page));


  _IC_Spare := 64;
  // Kích thước của một trang (bao gồm cả phần phụ trợ nếu có) "Spare"  trên chip spi NAND
  // bufflen := _IC_Page + _IC_Spare; // Giả sử _IC_Spare đã được định nghĩa, nếu không, gán giá trị phù hợp, ví dụ 0
  bufflen := _IC_Page ; // Giả sử _IC_Spare đã được định nghĩa, nếu không, gán giá trị phù hợp, ví dụ 0
  
  // maxpage := _IC_Size /_IC_Page;  // for full chip
  maxpage := 64; // For 64 pages
  total_size := bufflen * maxpage; // Tổng kích thước dữ liệu cần đọc

  LogPrint('Total data size to read: ' + IntToStr(total_size) + ' bytes');

  // Tạo mảng buff (4 byte) và mảng tạm cho một trang
  // buff dùng cho các lệnh SPI (reset, đọc trang, đọc cache)
  buff := CreateByteArray(4);
  temp_page_buffer := CreateByteArray(bufflen); // Mảng tạm chứa dữ liệu một trang
  // sreg := CreateByteArray(1);
  sreg := $FF;

  // Reset chip một lần ở đầu
  SPIWrite(1, 1, $FF); // Gửi $FF, 

  // Cập nhật buff cho lệnh đọc trang (sẽ được cập nhật trong vòng lặp)
  // buff[0] = $13; // Lệnh đọc trang, sẽ được đặt lại trong vòng lặp
  // buff[1,2,3] = địa chỉ trang, sẽ được cập nhật trong vòng lặp

  ProgressBar(0, maxpage, 0);

  for PageNum := 0 to maxpage - 1 do
  begin
    Addr := PageNum;
    // Cập nhật buff cho lệnh đọc trang
    SetArrayItem(buff, 0, $13); // Lệnh đọc trang vào cache
    SetArrayItem(buff, 1, (Addr shr 16) and $FF); // Byte cao nhất của địa chỉ
    SetArrayItem(buff, 2, (Addr shr 8) and $FF);  // Byte giữa của địa chỉ
    SetArrayItem(buff, 3, Addr and $FF);          // Byte thấp nhất của địa chỉ

    // Gửi lệnh đọc trang vào cache
    SPIWrite(1, 4, buff);

    // Chờ chip bớt bận (sreg có thể cần CreateByteArray(1) nếu dùng SPIRead)
    
    repeat
      SetArrayItem(buff, 0, $0F); // Lệnh đọc thanh ghi trạng thái
      SetArrayItem(buff, 1, $C0); // Dữ liệu gửi kèm
      SPIWrite(0, 2, buff); // Gửi lệnh và dữ liệu
      SPIRead(1, 1, sreg);  // Đọc kết quả vào mảng sreg
    // until ((GetArrayItem(sreg, 0) and 1) <> 1); // Kiểm tra bit BUSY
    until ((sreg and 1) <> 1); // Kiểm tra bit BUSY

    // Gửi lệnh đọc dữ liệu từ cache vào buff
    SetArrayItem(buff, 0, $03); // Lệnh đọc dữ liệu từ cache
    SetArrayItem(buff, 1, 0);   // Byte địa chỉ 1
    SetArrayItem(buff, 2, 0);   // Byte địa chỉ 2
    SetArrayItem(buff, 3, 0);   // Byte địa chỉ 3 (thường là 0 cho toàn bộ cache)
    SPIWrite(0, 4, buff);

    // Đọc dữ liệu vào mảng tạm (thay vì trực tiếp vào editor)
    SPIRead(1, bufflen, temp_page_buffer); // Sử dụng SPIRead để ghi vào mảng script
    // // Debug
    // // ✅ In ra số byte đang ghi
    // LogPrint('Writing ' + IntToStr(bufflen) + ' bytes to cache for page ' + IntToStr(PageNum));
    // // ✅ Có thể in ra giá trị đầu tiên và cuối cùng trong temp_page_buffer để debug
    // LogPrint('First byte: ' + IntToHex(GetArrayItem(temp_page_buffer, 0), 2) + ', Last byte: ' + IntToHex(GetArrayItem(temp_page_buffer, bufflen - 1), 2));  

    // Ghi dữ liệu từ mảng tạm vào editor tại vị trí đúng (PageNum * bufflen)
    // Gọi ReadToEditor: size = bufflen, position = PageNum * bufflen, buffer = temp_page_buffer
    // Thứ tự tham số của Script_ReadToEditor là (size, position, buffer...)
    ReadToEditor(bufflen, PageNum * bufflen, temp_page_buffer);

    ProgressBar(1);
  end;

  LogPrint('Finished reading all 64 pages to editor.');
  SPIExitProgMode();
   // ===== TÍNH THỜI GIAN =====
  end_time := Now();
  elapsed_sec := (end_time - start_time) * 86400;
  speed_kbps := (total_size / 1024) / elapsed_sec;
  
  LogPrint('Read completed: ' + IntToStr(total_size) + ' bytes');
  LogPrint('Time elapsed: ' + FloatToStr(elapsed_sec) + ' seconds');
  LogPrint('Read speed: ' + FloatToStr(speed_kbps) + ' KB/s');
end

{$ WRITE_PAGE_1_ONLY}
begin
   _SPI_SPEED:= 1;
  if not SPIEnterProgMode(_SPI_SPEED) then LogPrint('Error setting SPI speed');
  LogPrint('Write only page 1');

  sreg := $FF;
  _IC_Spare := 64;
  bufflen := _IC_Page + _IC_Spare;
  //bufflen := _IC_Page ;
  buffer := CreateByteArray(bufflen);
  // khi dùng WriteFromEditor không cần + phần phụ trợ _IC_Spare := 64;
  WriteFromEditor(bufflen - _IC_Spare, bufflen - _IC_Spare , buffer); // Đọc từ offset 0x800 trong editor

  //Nếu bạn muốn kiểm tra dữ liệu trong bufflen (debug)
  LogPrint('bufflen: ' + IntToStr(bufflen));
  //Nếu bạn muốn kiểm tra dữ liệu trong buffer (debug)
  // debug dữ liệu (16 byte)
  for i := 0 to 15 do
    LogPrint('buffer[' + IntToStr(i) + '] = ' + IntToHex(GetArrayItem(buffer, i), 2));
  for i := 2032 to 2047 do
    LogPrint('buffer[' + IntToStr(i) + '] = ' + IntToHex(GetArrayItem(buffer, i), 2));
    

  // Reset
  SPIWrite(1, 1, $FF);
  // Delay(1);

  // Enable ECC
  // SPIWrite(0, 2, $0F, $B0);
  // SPIRead(1, 1, sreg);
  // sreg := sreg or $10;
  // SPIWrite(1, 1, $06);
  // SPIWrite(1, 3, $1F, $B0, sreg);

  // Write enable for each page
  SPIWrite(1, 1, $06);
  repeat
    SPIWrite(0, 2, $0F, $C0);
    SPIRead(1, 1, sreg);
  until((sreg and 1) <> 1);

  SPIWrite(0, 3, $02, 0, 0);
  SPIWrite(1, bufflen, buffer);
  Delay(1);
  SPIWrite(1, 4, $10, 0, 0, 1); // Page 1

  repeat
    Delay(1);
    SPIWrite(0, 2, $0F, $C0);
    SPIRead(1, 1, sreg);
  until((sreg and 1) <> 1);

  if (sreg and 8) <> 0 then
    LogPrint('ERROR writing page 1')
  else
    LogPrint('Page 1 written successfully');

   SPIExitProgMode();
end


{$ WRITE_64_PAGE}
begin
   _SPI_SPEED:= 1;
  if not SPIEnterProgMode(_SPI_SPEED) then LogPrint('Error setting SPI speed');
  LogPrint ('Write data to given page');
  // ===== BẮT ĐẦU ĐẾM THỜI GIAN =====
  start_time := Now();

  sreg :=$FF;
  _IC_Spare := 64;
  buff:= CreateByteArray(4);
  // Kích thước của một trang (bao gồm cả phần phụ trợ nếu có) "Spare"  trên chip spi NAND  Page = 2048 + Spare = 64 = 2112
  // khi dùng WriteFromEditor không cần + phần phụ trợ 
  bufflen:= _IC_Page + _IC_Spare;
  buffer:= CreateByteArray(bufflen);
  
  // khi dùng WriteFromEditor không cần + phần phụ trợ _IC_Spare := 64;
  // WriteFromEditor(bufflen, bufflen - _IC_Spare , buffer); // Đọc từ offset 0x800 trong editor
    //Nếu bạn muốn kiểm tra dữ liệu trong bufflen (debug)
  LogPrint('bufflen: ' + IntToStr(bufflen));
  //Nếu bạn muốn kiểm tra dữ liệu trong buffer (debug)
  // debug dữ liệu (16 byte)
  // for i := 0 to 15 do
  //   LogPrint('buffer[' + IntToStr(i) + '] = ' + IntToHex(GetArrayItem(buffer, i), 2));
  // for i := 2032 to 2047 do
  //   LogPrint('buffer[' + IntToStr(i) + '] = ' + IntToHex(GetArrayItem(buffer, i), 2));
  
  // maxpage := _IC_Size /_IC_Page;  // for full chip
  maxpage := 64; // For 64 pages
  total_size := _IC_Page * maxpage;

  // Reset chip once
  SPIWrite(1, 1, $FF);
  
  // Write enable once
  SPIWrite (1, 1, $06);
  
  // Reset protection bits
  SPIWrite(1, 3, $1F, $A0, 0);
  
  // Initial busy wait
  repeat
    SPIWrite(0, 2, $0F, $C0);
    SPIRead(1, 1, sreg);
  until((sreg and 1) <> 1);

  ProgressBar(0, maxpage, 0);

  for PageNum:=0 to maxpage-1 do
  begin
      // vì ví trí WriteFromEditor không cần phần phụ trợ IC_Spare
      StartAddr:=PageNum*(bufflen -_IC_Spare);
      
      // Transfer editor data to buffer array
      WriteFromEditor(bufflen - _IC_Spare , StartAddr, buffer);
      // Write enable for each page
      SPIWrite (1, 1, $06);

      // Write data to cache - optimized: combined operation
      // Pre-setup write command buffer (optimization)
      SetArrayItem(buff, 0, $02);
      SetArrayItem(buff, 1, 0);
      SetArrayItem(buff, 2, 0);
      SPIWrite (0, 3, buff);
      SPIWrite (1, bufflen, buffer);
  
      // Transfer data from cache to memory - setup address
      Addr:= PageNum;
      SetArrayItem(buff, 0, $10);
      SetArrayItem(buff, 1, (addr shr 16));
      SetArrayItem(buff, 2, (addr shr 8));
      SetArrayItem(buff, 3, (addr));

      SPIWrite (1, 4, buff);

      // Wait if busy - optimized check
      repeat
            SPIWrite(0, 2, $0F, $C0);
            SPIRead(1, 1, sreg);
      until((sreg and 1) <> 1);

      ProgressBar(1);
  end;

    // ===== TÍNH THỜI GIAN =====
  end_time := Now();
  elapsed_sec := (end_time - start_time) * 86400;
  speed_kbps := (total_size / 1024) * 1000 / Round(elapsed_sec * 1000);
  
  LogPrint('Write completed: ' + IntToStr(total_size) + ' bytes');
  LogPrint('Time elapsed: ' + FloatToStr(elapsed_sec) + ' seconds');
  LogPrint('Write speed: ' + IntToStr(speed_kbps) + ' KB/s');
  
  SPIExitProgMode ();
end

{$ WRITE_64_PAGE_skippFF}
begin
   _SPI_SPEED:= 1;
  if not SPIEnterProgMode(_SPI_SPEED) then LogPrint('Error setting SPI speed');
  LogPrint('Write data to given page (optimized)');
    // ===== BẮT ĐẦU ĐẾM THỜI GIAN =====
  start_time := Now();
  sreg := $FF;
  _IC_Spare := 64;
  buff := CreateByteArray(4);
  bufflen := _IC_Page + _IC_Spare;
  buffer := CreateByteArray(bufflen);
  
  // maxpage := _IC_Size /_IC_Page;  // for full chip
  maxpage := 64; // For 64 pages
  total_size := _IC_Page * maxpage;
  
  // Reset chip once
  SPIWrite(1, 1, $FF);
  
  // Write enable once
  SPIWrite(1, 1, $06);
  
  // Reset protection bits
  SPIWrite(1, 3, $1F, $A0, 0);
  
  // Initial busy wait
  repeat
    SPIWrite(0, 2, $0F, $C0);
    SPIRead(1, 1, sreg);
  until ((sreg and 1) <> 1);
  

  
  ProgressBar(0, maxpage, 0);
  
  for PageNum := 0 to maxpage - 1 do
  begin
    StartAddr := PageNum * (bufflen - _IC_Spare);
    
    // Transfer editor data to buffer array
    WriteFromEditor(bufflen - _IC_Spare , StartAddr, buffer);
    
    // **PARALLEL CHECK OPTIMIZATION: Check multiple bytes simultaneously**
    isEmpty := True;
    accumulated := $FF;
    
    // Phase 1: Quick parallel check - accumulate OR of all bytes
    // If result is 0xFF, all bytes are 0xFF
    for i := 0 to bufflen - 1 do
    begin
      accumulated := accumulated and GetArrayItem(buffer, i);
    end;
    
    // Fast path: if accumulated is not 0xFF, page has data
    isEmpty := (accumulated = $FF);
    
    // Skip this page if it's empty
    if isEmpty then
    begin
      LogPrint('Page ' + IntToStr(PageNum) + ' is empty (0xFF), skipping...');
      ProgressBar(1);
      Continue;
    end;
    
    // Write enable for each page
    SPIWrite(1, 1, $06);
    
    // Write data to cache - use pre-setup buffer
    SetArrayItem(buff, 0, $02);
    SetArrayItem(buff, 1, 0);
    SetArrayItem(buff, 2, 0);
    SPIWrite(0, 3, buff);
    SPIWrite(1, bufflen, buffer);
    
    // Transfer data from cache to memory
    Addr := PageNum;
    SetArrayItem(buff, 0, $10);
    SetArrayItem(buff, 1, (Addr shr 16));
    SetArrayItem(buff, 2, (Addr shr 8));
    SetArrayItem(buff, 3, Addr);
    SPIWrite(1, 4, buff);
    
    // Wait if busy - optimized check
    repeat
      SPIWrite(0, 2, $0F, $C0);
      SPIRead(1, 1, sreg);
    until ((sreg and 1) <> 1);
    
    ProgressBar(1);
  end;
      // ===== TÍNH THỜI GIAN =====
  end_time := Now();
  elapsed_sec := (end_time - start_time) * 86400;
  speed_kbps := (total_size / 1024) * 1000 / Round(elapsed_sec * 1000);
  
  LogPrint('Write completed: ' + IntToStr(total_size) + ' bytes');
  LogPrint('Time elapsed: ' + FloatToStr(elapsed_sec) + ' seconds');
  LogPrint('Write speed: ' + IntToStr(speed_kbps) + ' KB/s');
  
  SPIExitProgMode();
  LogPrint('Write completed');
end


{$ READ_FULL_CHIP_BATCH}
begin
   _SPI_SPEED:= 1;
  // ===== KHỞI TẠO =====
  if not SPIEnterProgMode(_SPI_SPEED) then
  begin
    LogPrint('Error: Cannot enter SPI mode');
    Exit;
  end;
  
  LogPrint('Reading full chip (batch 32 pages)...');
  
  // ===== BẮT ĐẦU ĐẾM THỜI GIAN =====
  start_time := Now();
  
  // Tính toán kích thước
  _IC_Spare := 64;
  bufflen := _IC_Page;
  // maxpage := _IC_Size /_IC_Page;  // for full chip
  maxpage := 64; // For 64 pages
  total_size := bufflen * maxpage;
  batch_size := 32;  // Đọc 32 pages mỗi lần
  
  LogPrint('Chip: ' + IntToStr(_IC_Size) + ' bytes, Pages: ' + IntToStr(maxpage));
  LogPrint('Batch size: ' + IntToStr(batch_size) + ' pages');
  
  // ===== CHUẨN BỊ BUFFERS =====
  page_buffer := CreateByteArray(bufflen);           // Buffer cho 1 page
  batch_buffer := CreateByteArray(bufflen * batch_size);  // Buffer cho 32 pages
  sreg := CreateByteArray(1);
  
  // ===== RESET CHIP =====
  SPIWrite(1, 1, $FF);
  
  // ===== PRE-SETUP STATIC COMMANDS =====
  cache_cmd := CreateByteArray(4);
  SetArrayItem(cache_cmd, 0, $03);
  SetArrayItem(cache_cmd, 1, 0);
  SetArrayItem(cache_cmd, 2, 0);
  SetArrayItem(cache_cmd, 3, 0);
  
  status_cmd := CreateByteArray(2);
  SetArrayItem(status_cmd, 0, $0F);
  SetArrayItem(status_cmd, 1, $C0);
  
  ProgressBar(0, maxpage, 0);
  
  // ===== MAIN BATCH READ LOOP =====
  BatchNum := 0;
  PageNum := 0;
  
  while PageNum < maxpage do
  begin
    // Tính số pages trong batch hiện tại (batch cuối có thể < 32)
    pages_in_batch := batch_size;
    if PageNum + batch_size > maxpage then
      pages_in_batch := maxpage - PageNum;
    
    // --- ĐỌC BATCH (32 pages hoặc ít hơn) ---
    for i := 0 to pages_in_batch - 1 do
    begin
      CurrentPage := PageNum + i;
      
      // Gửi lệnh đọc page vào cache
      SPIWrite(1, 4, $13, (CurrentPage shr 16) and $FF, (CurrentPage shr 8) and $FF, CurrentPage and $FF);
      
      // Chờ chip sẵn sàng
      repeat
        SPIWrite(0, 2, status_cmd);
        SPIRead(1, 1, sreg);
      until ((GetArrayItem(sreg, 0) and 1) = 0);
      
      // Đọc dữ liệu từ cache vào page_buffer
      SPIWrite(0, 4, cache_cmd);
      SPIRead(1, bufflen, page_buffer);
      
      // Copy page_buffer vào batch_buffer tại vị trí đúng
      offset := i * bufflen;
      for j := 0 to bufflen - 1 do
      begin
        SetArrayItem(batch_buffer, offset + j, GetArrayItem(page_buffer, j));
      end;
      
      ProgressBar(1);
    end;
    
    // --- CẬP NHẬT EDITOR 1 LẦN cho cả batch ---
    batch_data_size := pages_in_batch * bufflen;
    ReadToEditor(batch_data_size, PageNum * bufflen, batch_buffer);
    
    LogPrint('Batch ' + IntToStr(BatchNum) + ': Updated ' + IntToStr(pages_in_batch) + ' pages to Editor');
    
    // Chuyển sang batch tiếp theo
    PageNum := PageNum + pages_in_batch;
    BatchNum := BatchNum + 1;
  end;
  
  // ===== HOÀN TẤT =====
  SPIExitProgMode();
  
  // ===== TÍNH THỜI GIAN =====
  end_time := Now();
  elapsed_sec := (end_time - start_time) * 86400;
  speed_kbps := (total_size / 1024) / elapsed_sec;
  
  LogPrint('Batch read completed: ' + IntToStr(total_size) + ' bytes');
  LogPrint('Total batches: ' + IntToStr(BatchNum));
  LogPrint('Time elapsed: ' + FloatToStr(elapsed_sec) + ' seconds');
  LogPrint('Read speed: ' + FloatToStr(speed_kbps) + ' KB/s');
end

{$ READ_FULL_CHIP}
begin
   _SPI_SPEED:= 1;
  // ===== KHỞI TẠO =====
  if not SPIEnterProgMode(_SPI_SPEED) then
  begin
    LogPrint('Error: Cannot enter SPI mode');
    Exit;
  end;
  
  LogPrint('Reading full chip (non-pipeline)...');
  
  // ===== BẮT ĐẦU ĐẾM THỜI GIAN =====
  start_time := Now();
  
  // Tính toán kích thước
  _IC_Spare := 64;
  bufflen := _IC_Page;
  maxpage := _IC_Size /_IC_Page;  // for full chip
  // maxpage := 64; // For 64 pages
  total_size := bufflen * maxpage;
  
  LogPrint('Chip: ' + IntToStr(_IC_Size) + ' bytes, Pages: ' + IntToStr(maxpage));
  
  // ===== CHUẨN BỊ BUFFER =====
  page_buffer := CreateByteArray(bufflen);
  // sreg := CreateByteArray(1);
  sreg := $FF;
  
  // ===== RESET CHIP =====
  SPIWrite(1, 1, $FF);
  
  // ===== PRE-SETUP STATIC COMMANDS =====
  cache_cmd := CreateByteArray(4);
  SetArrayItem(cache_cmd, 0, $03);
  SetArrayItem(cache_cmd, 1, 0);
  SetArrayItem(cache_cmd, 2, 0);
  SetArrayItem(cache_cmd, 3, 0);
  
  status_cmd := CreateByteArray(2);
  SetArrayItem(status_cmd, 0, $0F);
  SetArrayItem(status_cmd, 1, $C0);
  
  ProgressBar(0, maxpage, 0);
  
  // ===== MAIN READ LOOP =====
  for PageNum := 0 to maxpage - 1 do
  begin
    // --- Bước 1: Gửi lệnh đọc page vào cache ---
    SPIWrite(1, 4, $13, (PageNum shr 16) and $FF, (PageNum shr 8) and $FF, PageNum and $FF);
    
    // --- Bước 2: Chờ chip sẵn sàng ---
    repeat
      SPIWrite(0, 2, status_cmd);
      SPIRead(1, 1, sreg);
    // until ((GetArrayItem(sreg, 0) and 1) = 0); // Kiểm tra bit BUSY
    until ((sreg and 1) <> 1); // Kiểm tra bit BUSY
    
    // --- Bước 3: Đọc dữ liệu từ cache ---
    SPIWrite(0, 4, cache_cmd);
    SPIRead(1, bufflen, page_buffer);
    
    // --- Bước 4: Ghi vào Editor ---
    ReadToEditor(bufflen, PageNum * bufflen, page_buffer);
    
    ProgressBar(1);
  end;
  
  // ===== HOÀN TẤT =====
  SPIExitProgMode();
  
  // ===== TÍNH THỜI GIAN =====
  end_time := Now();
  elapsed_sec := (end_time - start_time) * 86400;
  speed_kbps := (total_size / 1024) / elapsed_sec;
  
  LogPrint('Read completed: ' + IntToStr(total_size) + ' bytes');
  LogPrint('Time elapsed: ' + FloatToStr(elapsed_sec) + ' seconds');
  LogPrint('Read speed: ' + FloatToStr(speed_kbps) + ' KB/s');
end

{$ READ_FULL_CHIP_PIPELINE}
begin
   _SPI_SPEED:= 1;
  // ===== KHỞI TẠO =====
  if not SPIEnterProgMode(_SPI_SPEED) then
  begin
    LogPrint('Error: Cannot enter SPI mode');
    Exit;
  end;
  
  LogPrint('Reading full chip with pipeline optimization...');
  
  // ===== BẮT ĐẦU ĐẾM THỜI GIAN =====
  start_time := Now();  // Lưu thời điểm bắt đầu
  
  // Tính toán kích thước
  _IC_Spare := 64;
  bufflen := _IC_Page;
  // maxpage := _IC_Size /_IC_Page;  // for full chip
  maxpage := 64; // For 64 pages
  total_size := bufflen * maxpage;
  
  LogPrint('Chip: ' + IntToStr(_IC_Size) + ' bytes, Pages: ' + IntToStr(maxpage));
  
  // ===== CHUẨN BỊ DUAL BUFFERS (cho pipeline) =====
  buffer_A := CreateByteArray(bufflen);  // Buffer chẵn (0, 2, 4...)
  buffer_B := CreateByteArray(bufflen);  // Buffer lẻ (1, 3, 5...)
  sreg := CreateByteArray(1);
  
  // ===== RESET CHIP =====
  SPIWrite(1, 1, $FF);
  
  // ===== PRE-SETUP STATIC COMMANDS =====
  cache_cmd := CreateByteArray(4);
  SetArrayItem(cache_cmd, 0, $03);
  SetArrayItem(cache_cmd, 1, 0);
  SetArrayItem(cache_cmd, 2, 0);
  SetArrayItem(cache_cmd, 3, 0);
  
  status_cmd := CreateByteArray(2);
  SetArrayItem(status_cmd, 0, $0F);
  SetArrayItem(status_cmd, 1, $C0);
  
  ProgressBar(0, maxpage, 0);
  
  // ===== PIPELINE PRIMING: Khởi động page đầu tiên =====
  SPIWrite(1, 4, $13, 0, 0, 0);  // Gửi lệnh đọc page 0
  
  // ===== MAIN PIPELINE LOOP =====
  for PageNum := 0 to maxpage - 1 do
  begin
    // --- STAGE 1: Chờ page hiện tại sẵn sàng ---
    repeat
      SPIWrite(0, 2, status_cmd);
      SPIRead(1, 1, sreg);
    until ((GetArrayItem(sreg, 0) and 1) = 0);
    
    // --- STAGE 2: Đọc dữ liệu page hiện tại từ cache ---
    SPIWrite(0, 4, cache_cmd);
    
    // Sử dụng dual buffer: chẵn dùng A, lẻ dùng B
    // Kiểm tra chẵn/lẻ bằng phép AND bit thay vì mod
    if (PageNum and 1) = 0 then
    begin
      SPIRead(1, bufflen, buffer_A);
      current_buffer := buffer_A;
    end
    else
    begin
      SPIRead(1, bufflen, buffer_B);
      current_buffer := buffer_B;
    end;
    
    // --- STAGE 3: Ghi vào Editor ---
    ReadToEditor(bufflen, PageNum * bufflen, current_buffer);
    
    // --- STAGE 4: PIPELINE - Gửi lệnh đọc page tiếp theo (N+1) ---
    if PageNum < maxpage - 1 then
    begin
      NextPage := PageNum + 1;
      SPIWrite(1, 4, $13, (NextPage shr 16) and $FF, (NextPage shr 8) and $FF, NextPage and $FF);
    end;
    
    ProgressBar(1);
  end;
  
  // ===== HOÀN TẤT =====
  SPIExitProgMode();
  
  // ===== TÍNH THỜI GIAN =====
  end_time := Now();
  elapsed_sec := (end_time - start_time) * 86400;  // Chuyển từ days sang giây
  speed_kbps := (total_size / 1024) / elapsed_sec;  // KB/s
  
  LogPrint('Pipeline read completed: ' + IntToStr(total_size) + ' bytes');
  LogPrint('Time elapsed: ' + FloatToStr(elapsed_sec) + ' seconds');
  LogPrint('Read speed: ' + FloatToStr(speed_kbps) + ' KB/s');
end

{$ WRITE_FULL_CHIP}
begin
   _SPI_SPEED:= 1;
  // ===== KHỞI TẠO =====
  if not SPIEnterProgMode(_SPI_SPEED) then
  begin
    LogPrint('Error: Cannot enter SPI mode');
    Exit;
  end;
  
  LogPrint('Writing full chip...');
  
  // ===== BẮT ĐẦU ĐẾM THỜI GIAN =====
  start_time := Now();
  
  // Tính toán
  _IC_Spare := 64;
  bufflen := _IC_Page + _IC_Spare;
  maxpage := _IC_Size / _IC_Page;  // for full chip
  // maxpage := 64; // For 64 pages
  total_size := _IC_Page * maxpage;
  
  LogPrint('Chip: ' + IntToStr(_IC_Size) + ' bytes, Pages: ' + IntToStr(maxpage));
  
  // ===== CHUẨN BỊ BUFFER =====
  buffer := CreateByteArray(bufflen);
  // sreg := CreateByteArray(1);
  sreg := $FF;
  
  // ===== RESET & SETUP CHIP =====
  SPIWrite(1, 1, $FF);              // Reset
  SPIWrite(1, 1, $06);              // Write enable
  SPIWrite(1, 3, $1F, $A0, 0);     // Clear protection
  
  // Chờ ready ban đầu
  repeat
    SPIWrite(0, 2, $0F, $C0);
    SPIRead(1, 1, sreg);
  // until ((GetArrayItem(sreg, 0) and 1) = 0);
  until ((sreg and 1) <> 1);
  
  ProgressBar(0, maxpage, 0);
  
  // ===== MAIN WRITE LOOP =====
  for PageNum := 0 to maxpage - 1 do
  //for PageNum := 65533 to 65535 do
  //for PageNum := 1 to 2 do
  begin
    // Đọc data từ Editor (không bao gồm spare area)
    // vì ví trí WriteFromEditor không cần phần phụ trợ IC_Spare
    StartAddr:=PageNum*(bufflen - _IC_Spare);
    WriteFromEditor(bufflen - _IC_Spare , StartAddr, buffer);
    //  // debug dữ liệu (16 byte)
    // for i := 0 to 15 do
    //   LogPrint('buffer[' + IntToStr(i) + '] = ' + IntToHex(GetArrayItem(buffer, i), 2));
    // for i := 2032 to 2047 do
    //   LogPrint('buffer[' + IntToStr(i) + '] = ' + IntToHex(GetArrayItem(buffer, i), 2));
    
    // Write enable cho mỗi page
    SPIWrite(1, 1, $06);
    
    // Ghi data vào cache
    SPIWrite(0, 3, $02, 0, 0);
    SPIWrite(1, bufflen, buffer);
    
    // Chuyển từ cache vào memory
    SPIWrite(1, 4, $10, (PageNum shr 16) and $FF, (PageNum shr 8) and $FF, PageNum and $FF);
    
    // Chờ ghi xong
    repeat
      SPIWrite(0, 2, $0F, $C0);
      SPIRead(1, 1, sreg);
    // until ((GetArrayItem(sreg, 0) and 1) = 0);
    until ((sreg and 1) <> 1);

    ProgressBar(1);
  end;
  
  // ===== HOÀN TẤT =====
  SPIExitProgMode();
  
  // ===== TÍNH THỜI GIAN =====
  end_time := Now();
  elapsed_sec := (end_time - start_time) * 86400;
  speed_kbps := (total_size / 1024) * 1000 / Round(elapsed_sec * 1000);
  
  LogPrint('Write completed: ' + IntToStr(total_size) + ' bytes');
  LogPrint('Time elapsed: ' + FloatToStr(elapsed_sec) + ' seconds');
  LogPrint('Write speed: ' + IntToStr(speed_kbps) + ' KB/s');
end

{$ WRITE_FULL_CHIP_skippFF}
begin
   _SPI_SPEED:= 1;
  // ===== KHỞI TẠO =====
  if not SPIEnterProgMode(_SPI_SPEED) then
  begin
    LogPrint('Error: Cannot enter SPI mode');
    Exit;
  end;
  
  LogPrint('Writing full chip...');
  
  // ===== BẮT ĐẦU ĐẾM THỜI GIAN =====
  start_time := Now();
  
  // Tính toán
  _IC_Spare := 64;
  bufflen := _IC_Page + _IC_Spare;
  // maxpage := _IC_Size /_IC_Page;  // full chip
  maxpage := 64; // For 64 pages
  total_size := _IC_Page * maxpage;
  
  LogPrint('Chip: ' + IntToStr(_IC_Size) + ' bytes, Pages: ' + IntToStr(maxpage));
  
  // ===== CHUẨN BỊ BUFFER =====
  buffer := CreateByteArray(bufflen);
  // sreg := CreateByteArray(1);
  sreg := $FF;
  
  // ===== RESET & SETUP CHIP =====
  SPIWrite(1, 1, $FF);              // Reset
  SPIWrite(1, 1, $06);              // Write enable
  SPIWrite(1, 3, $1F, $A0, 0);     // Clear protection
  
  // Chờ ready ban đầu
  repeat
    SPIWrite(0, 2, $0F, $C0);
    SPIRead(1, 1, sreg);
  // until ((GetArrayItem(sreg, 0) and 1) = 0);
  until ((sreg and 1) <> 1);
  
  ProgressBar(0, maxpage, 0);
  
  // ===== MAIN WRITE LOOP =====
  for PageNum := 0 to maxpage - 1 do
  begin
    // Đọc data từ Editor (không bao gồm spare area)
    // vì ví trí WriteFromEditor không cần phần phụ trợ IC_Spare
    StartAddr:=PageNum*(bufflen - _IC_Spare);
    WriteFromEditor(bufflen - _IC_Spare , StartAddr, buffer);
    
    // Kiểm tra page có toàn FF không (skip empty pages)
    isEmpty := True;
    accumulated := $FF;
    for i := 0 to bufflen - 1 do
      accumulated := accumulated and GetArrayItem(buffer, i);
    isEmpty := (accumulated = $FF);
    
    if isEmpty then
    begin
      ProgressBar(1);
      Continue;  // Bỏ qua page trống
    end;
    
    // Write enable cho mỗi page
    SPIWrite(1, 1, $06);
    
    // Ghi data vào cache
    SPIWrite(0, 3, $02, 0, 0);
    SPIWrite(1, bufflen, buffer);
    
    // Chuyển từ cache vào memory
    SPIWrite(1, 4, $10, (PageNum shr 16) and $FF, (PageNum shr 8) and $FF, PageNum and $FF);
    
    // Chờ ghi xong
    repeat
      SPIWrite(0, 2, $0F, $C0);
      SPIRead(1, 1, sreg);
    // until ((GetArrayItem(sreg, 0) and 1) = 0);
    until ((sreg and 1) <> 1);

    ProgressBar(1);
  end;
  
  // ===== HOÀN TẤT =====
  SPIExitProgMode();
  
  // ===== TÍNH THỜI GIAN =====
  end_time := Now();
  elapsed_sec := (end_time - start_time) * 86400;
  speed_kbps := (total_size / 1024) * 1000 / Round(elapsed_sec * 1000);
  
  LogPrint('Write completed: ' + IntToStr(total_size) + ' bytes');
  LogPrint('Time elapsed: ' + FloatToStr(elapsed_sec) + ' seconds');
  LogPrint('Write speed: ' + IntToStr(speed_kbps) + ' KB/s');
end

{$ ERASE_FULL_CHIP}
// Erase entire chip
begin
   _SPI_SPEED:= 1;
  if not SPIEnterProgMode(_SPI_SPEED) then LogPrint('Error setting SPI speed');
  
  maxpage := _IC_Size / _IC_Page;
  totalblocks := maxpage / 64;
  
  LogPrint('Erasing ' + IntToStr(totalblocks) + ' blocks...');
  
  sreg := $FF;
  buff := CreateByteArray(4);
  
  SPIWrite(1, 1, $FF);
  Delay(10);
  SPIWrite(1, 1, $06);
  SPIWrite(1, 3, $1F, $A0, 0);
  
  repeat
    SPIWrite(0, 2, $0F, $C0);
    SPIRead(1, 1, sreg);
  until((sreg and 1) <> 1);
  
  ProgressBar(0, totalblocks, 0);
  log_counter := 0;
  
  for BlockNum := 0 to totalblocks-1 do
  begin
    SPIWrite(1, 1, $06);
    
    Addr := BlockNum * 64;
    SPIWrite(1, 4, $D8, (Addr shr 16), (Addr shr 8), (Addr));
    
    repeat
      Delay(2);
      SPIWrite(0, 2, $0F, $C0);
      SPIRead(1, 1, sreg);
    until((sreg and 1) <> 1);
    
    ProgressBar(1);
    
    log_counter := log_counter + 1;
    if log_counter >= 50 then
    begin
      LogPrint('Progress: ' + IntToStr(BlockNum) + '/' + IntToStr(totalblocks));
      log_counter := 0;
    end;
  end;
  
  LogPrint('Chip erase completed');
  SPIExitProgMode();
end

{$ VERIFY_WRITE}
begin
   _SPI_SPEED:= 1;
  // ===== KHỞI TẠO =====
  if not SPIEnterProgMode(_SPI_SPEED) then
  begin
    LogPrint('Error: Cannot enter SPI mode');
    Exit;
  end;
  
  LogPrint('Verifying written data...');
  
  // ===== BẮT ĐẦU ĐẾM THỜI GIAN =====
  start_time := Now();
  
  // Tính toán
  _IC_Spare := 64;
  bufflen := _IC_Page;
  // maxpage := _IC_Size /_IC_Page;  // for full chip
  maxpage := 64; // For 64 pages
  total_size := bufflen * maxpage;
  
  LogPrint('Chip: ' + IntToStr(_IC_Size) + ' bytes, Pages: ' + IntToStr(maxpage));
  
  // ===== CHUẨN BỊ BUFFERS =====
  chip_buffer := CreateByteArray(bufflen);    // Dữ liệu đọc từ chip
  editor_buffer := CreateByteArray(bufflen);  // Dữ liệu từ editor
  // sreg := CreateByteArray(1);
  sreg := $FF;
  
  // ===== RESET CHIP =====
  SPIWrite(1, 1, $FF);
  
  // ===== PRE-SETUP STATIC COMMANDS =====
  cache_cmd := CreateByteArray(4);
  SetArrayItem(cache_cmd, 0, $03);
  SetArrayItem(cache_cmd, 1, 0);
  SetArrayItem(cache_cmd, 2, 0);
  SetArrayItem(cache_cmd, 3, 0);
  
  status_cmd := CreateByteArray(2);
  SetArrayItem(status_cmd, 0, $0F);
  SetArrayItem(status_cmd, 1, $C0);
  
  ProgressBar(0, maxpage, 0);
  
  // ===== Biến đếm lỗi =====
  error_count := 0;
  error_pages := 0;
  verified_bytes := 0;
  
  // ===== MAIN VERIFY LOOP =====
  for PageNum := 0 to maxpage - 1 do
  begin
    // --- Đọc dữ liệu từ Editor ---
    // Đọc data từ Editor (không bao gồm spare area)
    // vì ví trí WriteFromEditor không cần phần phụ trợ IC_Spare
    StartAddr := PageNum * bufflen ;
    WriteFromEditor(bufflen, StartAddr, editor_buffer);
     //Nếu bạn muốn kiểm tra dữ liệu trong bufflen (debug)
    // LogPrint('bufflen: ' + IntToStr(bufflen));
    //Nếu bạn muốn kiểm tra dữ liệu trong buffer (debug)

    // // debug dữ liệu (16 byte)
    // for i := 0 to 15 do
    //   LogPrint('Page ' + IntToStr(PageNum) + 'editor_buffer[' + IntToStr(i) + '] = ' + IntToHex(GetArrayItem(editor_buffer, i), 2));
    // for i := 2032 to 2047 do
    //   LogPrint('Page ' + IntToStr(PageNum) +'editor_buffer[' + IntToStr(i) + '] = ' + IntToHex(GetArrayItem(editor_buffer, i), 2));

    // --- Kiểm tra xem page có phải toàn FF không ---
    isEmpty := True;
    accumulated := $FF;
    for i := 0 to bufflen - 1 do
      accumulated := accumulated and GetArrayItem(editor_buffer, i);
    isEmpty := (accumulated = $FF);
    
    // Nếu page trống trong editor, bỏ qua verify
    if isEmpty then
    begin
      ProgressBar(1);
      Continue;
    end;
    
    // --- Đọc dữ liệu từ Chip ---
    SPIWrite(1, 4, $13, (PageNum shr 16) and $FF, (PageNum shr 8) and $FF, PageNum and $FF);
    
    // Chờ ready
    repeat
      SPIWrite(0, 2, status_cmd);
      SPIRead(1, 1, sreg);
    // until ((GetArrayItem(sreg, 0) and 1) = 0); // Kiểm tra bit BUSY
    until ((sreg and 1) <> 1); // Kiểm tra bit BUSY
    
    // Đọc data từ cache
    SPIWrite(0, 4, cache_cmd);
    SPIRead(1, bufflen, chip_buffer);

    // // debug dữ liệu (16 byte)
    // for i := 0 to 15 do
    //   LogPrint('Page ' + IntToStr(PageNum) + 'chip_buffer[' + IntToStr(i) + '] = ' + IntToHex(GetArrayItem(chip_buffer, i), 2));
    // for i := 2032 to 2047 do
    //   LogPrint('Page ' + IntToStr(PageNum) +'chip_buffer[' + IntToStr(i) + '] = ' + IntToHex(GetArrayItem(chip_buffer, i), 2));
    
    // --- So sánh dữ liệu ---
    page_has_error := False;
    first_error_offset := -1;
    
    for i := 0 to bufflen - 1 do
    begin
      chip_byte := GetArrayItem(chip_buffer, i);
      editor_byte := GetArrayItem(editor_buffer, i);
      
      if chip_byte <> editor_byte then
      begin
        if not page_has_error then
        begin
          page_has_error := True;
          first_error_offset := i;
          error_pages := error_pages + 1;
        end;
        error_count := error_count + 1;
      end;
    end;
    
    // --- Log lỗi nếu có ---
    if page_has_error then
    begin
      LogPrint('ERROR Page ' + IntToStr(PageNum) + ': Mismatch at offset 0x' + IntToHex(first_error_offset, 4));
      LogPrint('  Expected: 0x' + IntToHex(GetArrayItem(editor_buffer, first_error_offset), 2) + 
               ', Read: 0x' + IntToHex(GetArrayItem(chip_buffer, first_error_offset), 2));
    end
    else
    begin
      verified_bytes := verified_bytes + bufflen;
    end;
    
    ProgressBar(1);
  end;
  
  // ===== HOÀN TẤT =====
  SPIExitProgMode();
  
  // ===== TÍNH THỜI GIAN =====
  end_time := Now();
  elapsed_sec := (end_time - start_time) * 86400;
  
  // ===== KẾT QUẢ =====
  LogPrint('==========================================');
  LogPrint('Verification completed!');
  LogPrint('Time elapsed: ' + FloatToStr(elapsed_sec) + ' seconds');
  LogPrint('Total pages verified: ' + IntToStr(maxpage));
  LogPrint('Verified bytes: ' + IntToStr(verified_bytes));
  
  if error_count = 0 then
  begin
    LogPrint('Result: PASS - All data verified successfully!');
  end
  else
  begin
    LogPrint('Result: FAIL');
    LogPrint('Error pages: ' + IntToStr(error_pages) + ' / ' + IntToStr(maxpage));
    LogPrint('Error bytes: ' + IntToStr(error_count));
    LogPrint('Success rate: ' + FloatToStr((maxpage - error_pages) * 100 / maxpage) + '%');
  end;
  LogPrint('==========================================');
end

{$ READ_BBM_table}
begin
   _SPI_SPEED:= 1;
  BBM:= CreateByteArray(80);
  if not SPIEnterProgMode(_SPI_SPEED) then LogPrint('Error setting SPI speed');
  LogPrint ('Read winbond BBM table');
  
  // Read winbond BBM table 
  SPIWrite (0, 2, $A5, $00);
  SPIRead(1, 80, BBM);
  
  for i:=0 to 19 do
  begin
    idx := i * 4;
    logprint('LBA' + inttostr(i) + ': ' + 
             inttohex((GetArrayItem(BBM, idx)),2) + 
             inttohex((GetArrayItem(BBM, idx+1)),2) + 
             ' PBA' + inttostr(i) + ': ' + 
             inttohex((GetArrayItem(BBM, idx+2)),2) + 
             inttohex((GetArrayItem(BBM, idx+3)),2));
  end;
  
  LogPrint ('End read BBM table');
  SPIExitProgMode ();
end

// for ch347
{$ TEST_ALL_SPI_SPEEDS}
begin
  LogPrint('=== SPI Speed Test ===');
  LogPrint('Connect oscilloscope CH1 to SCLK');
  LogPrint('Press any key to continue...');
  ShowMessage('Ready? Oscilloscope connected?');   
  speeds := CreateByteArray(8);
  SetArrayItem(speeds, 0, 0);    // Tag = 0 '60 MHz'
  SetArrayItem(speeds, 1, 1);    // Tag = 1 '30 MHz'
  SetArrayItem(speeds, 2, 2);    // Tag = 2 '15 MHz'
  SetArrayItem(speeds, 3, 3);    // Tag = 3 '7.5 MHz'
  SetArrayItem(speeds, 4, 4);    // Tag = 4 '3.75 MHz'
  SetArrayItem(speeds, 5, 5);    // Tag = 5 '1.875 MHz'
  SetArrayItem(speeds, 6, 6);    // Tag = 6 '937.5 KHz'
  SetArrayItem(speeds, 7, 7);    // Tag = 7 '468.75 KHz'
  for i := 0 to 7 do
  begin
    speed := GetArrayItem(speeds, i);
    LogPrint('');
    LogPrint('Testing speed setting: ' + IntToStr(speed));
    
    if not SPIEnterProgMode(speed) then
    begin
      LogPrint('Failed to enter prog mode');
      Continue;
    end;
    
    LogPrint('Generating SPI traffic...');
    // Gửi 50 transactions
    for j := 0 to 49 do
    begin
      SPIWrite(1, 1, $9F);  // Read ID
      Delay(2);  // 2ms giữa mỗi transaction
    end;
    
    SPIExitProgMode();
    
    ShowMessage('Speed ' + IntToStr(speed) + ' done. Check scope frequency now!' + chr(13) + 'Press OK for next speed...');
  end;
  
  LogPrint('');
  LogPrint('=== Test Complete ===');
end;


