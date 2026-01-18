# SPI NAND Address - Single-plane vs Multi-plane

## 1. CẤU TRÚC CHIP

### W25N01GV (Single-plane - 128MB)
```
┌─────────────────────────────────────────────────────────┐
│                      1 PLANE                            │
│  ┌─────┐ ┌─────┐ ┌─────┐         ┌─────┐               │
│  │Blk 0│ │Blk 1│ │Blk 2│  ...    │Blk  │               │
│  │     │ │     │ │     │         │1023 │               │
│  │64 pg│ │64 pg│ │64 pg│         │64 pg│               │
│  └─────┘ └─────┘ └─────┘         └─────┘               │
│                                                         │
│  Total: 1024 blocks × 64 pages = 65,536 pages          │
└─────────────────────────────────────────────────────────┘
```

### MT29F2G01 (Multi-plane - 256MB)
```
┌─────────────────────────────────────────────────────────┐
│  PLANE 0 (Block chẵn)          PLANE 1 (Block lẻ)      │
│  ┌─────┐ ┌─────┐ ┌─────┐      ┌─────┐ ┌─────┐ ┌─────┐  │
│  │Blk 0│ │Blk 2│ │Blk 4│ ...  │Blk 1│ │Blk 3│ │Blk 5│  │
│  │     │ │     │ │     │      │     │ │     │ │     │  │
│  │64 pg│ │64 pg│ │64 pg│      │64 pg│ │64 pg│ │64 pg│  │
│  └─────┘ └─────┘ └─────┘      └─────┘ └─────┘ └─────┘  │
│                                                         │
│  Total: 2048 blocks × 64 pages = 131,072 pages         │
│  Mỗi plane có cache riêng (2KB + 128B spare)           │
└─────────────────────────────────────────────────────────┘
```

---

## 2. ROW ADDRESS (GetRowAddressBytes)

Row Address xác định **PAGE nào** trong chip → dùng cho Page Read, Program Execute, Block Erase

### Cấu trúc 3 bytes (24 bit):
```
         addr_bytes[0]      addr_bytes[1]      addr_bytes[2]
        ┌───────────┐      ┌───────────┐      ┌───────────┐
        │  Bit 23-16│      │  Bit 15-8 │      │  Bit 7-0  │
        │  (MSB)    │      │  (Middle) │      │  (LSB)    │
        └───────────┘      └───────────┘      └───────────┘
```

### Ví dụ cụ thể:

| PageAddr | Binary (24-bit) | addr[0] | addr[1] | addr[2] |
|----------|-----------------|---------|---------|---------|
| 0        | 0000 0000 0000 0000 0000 0000 | 0x00 | 0x00 | 0x00 |
| 64       | 0000 0000 0000 0000 0100 0000 | 0x00 | 0x00 | 0x40 |
| 256      | 0000 0000 0000 0001 0000 0000 | 0x00 | 0x01 | 0x00 |
| 65535    | 0000 0000 1111 1111 1111 1111 | 0x00 | 0xFF | 0xFF |

### Code:
```pascal
addr_bytes[0] := (PageAddr shr 16) and $FF;  // Bit 23-16
addr_bytes[1] := (PageAddr shr 8) and $FF;   // Bit 15-8
addr_bytes[2] := PageAddr and $FF;           // Bit 7-0
```

**→ GIỐNG NHAU cho cả Single-plane và Multi-plane!**

---

## 3. COLUMN ADDRESS (GetColumnAddress)

Column Address xác định **VỊ TRÍ BYTE** trong page cache → dùng cho Program Load, Read from Cache

### W25N01GV (Single-plane):
```
Column Address (2 bytes = 16 bit):
┌─────────────────────────────────────────────────────────┐
│ Bit 15  14  13  12 │ 11  10   9   8 │  7   6   5   4   3   2   1   0 │
├────────────────────┼────────────────┼─────────────────────────────────┤
│    Dummy (0)       │       Column Address (0-2111)                   │
│    Không dùng      │       Offset trong page (2048 + 64 spare)       │
└────────────────────┴────────────────┴─────────────────────────────────┘

Ví dụ: Đọc từ đầu page → Column = 0x0000
       cmd_buff[1] = 0x00  (high byte)
       cmd_buff[2] = 0x00  (low byte)
```

### MT29F2G01 (Multi-plane):
```
Column Address (2 bytes = 16 bit):
┌─────────────────────────────────────────────────────────┐
│ Bit 15  14  13 │ 12 │ 11  10   9   8   7   6   5   4   3   2   1   0 │
├────────────────┼────┼─────────────────────────────────────────────────┤
│   Dummy (0)    │ P  │         Column Address (0-2175)                 │
│   Không dùng   │ L  │         Offset trong page (2048 + 128 spare)    │
│                │ A  │                                                 │
│                │ N  │                                                 │
│                │ E  │                                                 │
└────────────────┴────┴─────────────────────────────────────────────────┘
                   ↑
                   BIT 12 = PLANE SELECT
                   0 = Plane 0 (block chẵn)
                   1 = Plane 1 (block lẻ)
```

---

## 4. CÁCH XÁC ĐỊNH PLANE TỪ PageAddr

```
PageAddr → Block Number → Plane Select

┌──────────────┬─────────────┬─────────────┬────────────────┐
│   PageAddr   │ Block       │ Block & 1   │ Plane          │
│              │ (PageAddr   │ (LSB của    │                │
│              │  div 64)    │  Block)     │                │
├──────────────┼─────────────┼─────────────┼────────────────┤
│    0 - 63    │     0       │     0       │ Plane 0        │
│   64 - 127   │     1       │     1       │ Plane 1        │
│  128 - 191   │     2       │     0       │ Plane 0        │
│  192 - 255   │     3       │     1       │ Plane 1        │
│  256 - 319   │     4       │     0       │ Plane 0        │
│  320 - 383   │     5       │     1       │ Plane 1        │
│     ...      │    ...      │    ...      │    ...         │
└──────────────┴─────────────┴─────────────┴────────────────┘
```

### Code:
```pascal
BlockNum := PageAddr div PagesPerBlock;   // = PageAddr div 64
PlaneSelect := BlockNum and $01;          // 0 hoặc 1
Result := word(PlaneSelect) shl 12;       // Đặt vào bit 12
```

---

## 5. VÍ DỤ CỤ THỂ - ĐỌC PAGE 150

### Bước 1: Tính toán
```
PageAddr = 150
Block = 150 div 64 = 2  (Block chẵn)
Plane = 2 and 1 = 0     (Plane 0)
```

### Bước 2: Row Address (cho Page Read 13h)
```
PageAddr = 150 = 0x000096

addr_bytes[0] = 0x00
addr_bytes[1] = 0x00
addr_bytes[2] = 0x96

Lệnh gửi: [13h] [00h] [00h] [96h]
```

### Bước 3: Column Address (cho Read Cache 03h)

**Single-plane (W25N01GV):**
```
Column = 0x0000

cmd_buff = [03h] [00h] [00h] [00h dummy]
                  ↑     ↑
               Col_H  Col_L
```

**Multi-plane (MT29F2G01):**
```
Plane = 0 → Column = 0 << 12 = 0x0000

cmd_buff = [03h] [00h] [00h] [00h dummy]
                  ↑     ↑
               Col_H  Col_L (Plane bit = 0)
```

---

## 6. VÍ DỤ CỤ THỂ - GHI PAGE 100

### Bước 1: Tính toán
```
PageAddr = 100
Block = 100 div 64 = 1  (Block lẻ)
Plane = 1 and 1 = 1     (Plane 1) ★
```

### Bước 2: Column Address (cho Program Load 02h)

**Single-plane (W25N01GV):**
```
Column = 0x0000

cmd_buff = [02h] [00h] [00h]
                  ↑     ↑
               Col_H  Col_L
```

**Multi-plane (MT29F2G01):**
```
Plane = 1 → Column = 1 << 12 = 0x1000

        Bit:  15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
             ┌──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┐
             │ 0│ 0│ 0│ 1│ 0│ 0│ 0│ 0│ 0│ 0│ 0│ 0│ 0│ 0│ 0│ 0│
             └──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┘
                      ↑
                   PLANE BIT = 1

Column High = 0x10
Column Low  = 0x00

cmd_buff = [02h] [10h] [00h]
                  ↑     ↑
               Col_H  Col_L (Plane bit = 1)
```

### Bước 3: Row Address (cho Program Execute 10h)
```
PageAddr = 100 = 0x000064

addr_bytes[0] = 0x00
addr_bytes[1] = 0x00
addr_bytes[2] = 0x64

Lệnh gửi: [10h] [00h] [00h] [64h]
```

---

## 7. TẠI SAO MULTI-PLANE CẦN PLANE BIT Ở COLUMN?

```
MT29F2G01 có 2 cache buffers riêng biệt:

┌─────────────────────────────────────────────────────────┐
│                     SPI NAND CHIP                       │
│  ┌─────────────────┐      ┌─────────────────┐          │
│  │   PLANE 0       │      │   PLANE 1       │          │
│  │   CACHE         │      │   CACHE         │          │
│  │   (2176 bytes)  │      │   (2176 bytes)  │          │
│  └────────┬────────┘      └────────┬────────┘          │
│           │                        │                    │
│           ▼                        ▼                    │
│  ┌─────────────────┐      ┌─────────────────┐          │
│  │ Block 0,2,4...  │      │ Block 1,3,5...  │          │
│  │ (Flash Array)   │      │ (Flash Array)   │          │
│  └─────────────────┘      └─────────────────┘          │
└─────────────────────────────────────────────────────────┘

Khi đọc/ghi:
- Page Read (13h): Load data từ Flash → Cache của plane tương ứng
- Read Cache (03h): Đọc từ Cache → cần chỉ định CACHE NÀO (Plane bit)
- Program Load (02h): Ghi vào Cache → cần chỉ định CACHE NÀO (Plane bit)
- Program Exec (10h): Ghi từ Cache → Flash (Row Address xác định plane)
```

---

## 8. TỔNG KẾT

| Thành phần | Single-plane | Multi-plane | Khác biệt |
|------------|--------------|-------------|-----------|
| Row Address | 24-bit linear | 24-bit linear | **Giống nhau** |
| Column Address | 12-bit (0-2047) | 12-bit + Plane bit | **Bit 12 = Plane** |
| Page Read | [13h][Row 3B] | [13h][Row 3B] | Giống |
| Read Cache | [03h][Col 2B][Dummy] | [03h][**P**+Col 2B][Dummy] | **Khác** |
| Program Load | [02h][Col 2B] | [02h][**P**+Col 2B] | **Khác** |
| Program Exec | [10h][Row 3B] | [10h][Row 3B] | Giống |
| Block Erase | [D8h][Row 3B] | [D8h][Row 3B] | Giống |
