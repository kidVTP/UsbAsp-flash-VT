unit findchip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, dom, utilfunc, lazUTF8;

type

  { TChipSearchForm }

  TChipSearchForm = class(TForm)
    Bevel1: TBevel;
    ChipSearchSelectButton: TButton;
    EditSearch: TEdit;
    Label1: TLabel;
    ListBoxChips: TListBox;
    procedure ChipSearchSelectButtonClick(Sender: TObject);
    procedure EditSearchChange(Sender: TObject);
    procedure ListBoxChipsDblClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  procedure FindChip(XMLfile: TXMLDocument; chipname: string; chipid: string = '');
  procedure SelectChip(XMLfile: TXMLDocument; chipname: string);

var
  ChipSearchForm: TChipSearchForm;

implementation

uses main, scriptsfunc;

{$R *.lfm}

//Ищет чип по имени. Если id не пустое то только по id.
procedure FindChip(XMLfile: TXMLDocument; chipname: string; chipid: string = '');
var
  Node, ChipNode: TDOMNode;
  j, i: integer;
  cs: string;
begin
  if XMLfile <> nil then
  begin
    Node := XMLfile.DocumentElement.FirstChild;

    while Assigned(Node) do
    begin
     //Node.NodeName; //Раздел(SPI, I2C...)

     // Используем свойство ChildNodes
     with Node.ChildNodes do
     try
       for j := 0 to (Count - 1) do
       begin
         //Item[j].NodeName; //Раздел Фирма

         for i := 0 to (Item[j].ChildNodes.Count - 1) do
         begin

           ChipNode := Item[j].ChildNodes.Item[i];
           if chipid <> '' then
           begin
             if (ChipNode.HasAttributes) then
               if  ChipNode.Attributes.GetNamedItem('id') <> nil then
               begin
                 cs := UTF16ToUTF8(ChipNode.Attributes.GetNamedItem('id').NodeValue); //id
                 if Upcase(cs) = Upcase(chipid) then
                   ChipSearchForm.ListBoxChips.Items.Append(UTF16ToUTF8(ChipNode.NodeName)+' ('+ UTF16ToUTF8(Item[j].NodeName) +')');
               end;
           end
           else
           begin
             cs := UTF16ToUTF8(ChipNode.NodeName); //Чип
             if pos(Upcase(chipname), Upcase(cs)) > 0 then
               ChipSearchForm.ListBoxChips.Items.Append(cs+' ('+ UTF16ToUTF8(Item[j].NodeName) +')');
           end;

         end;
       end;
     finally
       Free;
     end;
     Node := Node.NextSibling;
    end;
  end;
end;


// Trong findchip.pas
procedure SelectChip(XMLfile: TXMLDocument; chipname: string);
var
  Node, ChipNode: TDOMNode;
  j, i: integer;
  cs: string;
  spicmdValue: string; // Biến tạm để lưu giá trị spicmd
begin
  if XMLfile <> nil then
  begin
    Node := XMLfile.DocumentElement.FirstChild;

    while Assigned(Node) do
    begin
     //Node.NodeName; //Раздел(SPI, I2C...)

     // Используем свойство ChildNodes
     with Node.ChildNodes do
     try
       for j := 0 to (Count - 1) do
       begin
         //Item[j].NodeName; //Раздел Фирма

         for i := 0 to (Item[j].ChildNodes.Count - 1) do
         begin
           cs := UTF16ToUTF8(Item[j].ChildNodes.Item[i].NodeName); //Чип
           if Upcase(chipname) = Upcase(cs) then
           begin
             ChipNode := Item[j].ChildNodes.Item[i];
             Main.CurrentICParam.Name:= UTF16ToUTF8(ChipNode.NodeName);
             if (ChipNode.HasAttributes) then
             begin
               // Lấy giá trị spicmd (nếu có) để xử lý
               spicmdValue := '';
               if ChipNode.Attributes.GetNamedItem('spicmd') <> nil then
                 spicmdValue := UpperCase(UTF16ToUTF8(ChipNode.Attributes.GetNamedItem('spicmd').NodeValue));

               // --- CẬP NHẬT LOGIC XỬ LÝ SPICMD ---
               if spicmdValue <> '' then
               begin
                 if spicmdValue = 'KB' then
                   Main.CurrentICParam.SpiCmd:= SPI_CMD_KB
                 else if spicmdValue = '45' then
                   Main.CurrentICParam.SpiCmd:= SPI_CMD_45
                 else if spicmdValue = '25' then
                   Main.CurrentICParam.SpiCmd:= SPI_CMD_25
                 else if spicmdValue = '95' then
                   Main.CurrentICParam.SpiCmd:= SPI_CMD_95
                 else if spicmdValue = '25N' then // <-- Thêm dòng này cho SPI NAND
                   Main.CurrentICParam.SpiCmd:= SPI_CMD_25_NAND // <-- Gán hằng số bạn đã định nghĩa
                 else
                   // Nếu có spicmd nhưng không khớp, có thể gán mặc định hoặc giữ nguyên
                   // Ví dụ: gán mặc định là 25 nếu không nhận diện được
                   Main.CurrentICParam.SpiCmd:= SPI_CMD_25; // hoặc không gán gì
               end
               else // Nếu KHÔNG có thuộc tính 'spicmd'
               // Thì mặc định là SPI_CMD_25 (giữ nguyên logic cũ)
               if (ChipNode.Attributes.GetNamedItem('addrtype') = nil) and
                     (ChipNode.Attributes.GetNamedItem('addrbitlen') = nil) then
                     begin
                        Main.CurrentICParam.SpiCmd:= SPI_CMD_25;
                     end
               else
                 // Nếu có addrtype hoặc addrbitlen, chip là I2C hoặc MW, SpiCmd sẽ được ghi đè sau
                 // hoặc giữ nguyên giá trị hiện tại (nếu có) hoặc gán 0 nếu không có gì.
                 // Không cần gán gì cụ thể ở đây nếu bạn xử lý sau.
                 ; // hoặc Main.CurrentICParam.SpiCmd:= 0; nếu muốn rõ ràng

               // Cập nhật ComboSPICMD và UI dựa trên SpiCmd đã xác định
               // Chỉ cập nhật UI nếu là SPI (dựa trên việc addrtype và addrbitlen không tồn tại)
               if (ChipNode.Attributes.GetNamedItem('addrtype') = nil) and
                  (ChipNode.Attributes.GetNamedItem('addrbitlen') = nil) then
               begin
                 MainForm.ComboSPICMD.ItemIndex := CurrentICParam.SpiCmd;
                 MainForm.RadioSPI.Checked:= true;
                 MainForm.RadioSPIChange(MainForm);
               end;

               // ... (phần còn lại của hàm giữ nguyên) ...
               // Các phần khác của hàm xử lý page, size, script, addrtype, addrbitlen không thay đổi
               if ChipNode.Attributes.GetNamedItem('addrbitlen') <> nil then
               begin
                 MainForm.RadioMw.Checked:= true;
                 Main.CurrentICParam.MWAddLen := StrToInt(UTF16ToUTF8(ChipNode.Attributes.GetNamedItem('addrbitlen').NodeValue));
               end
               else
                 Main.CurrentICParam.MWAddLen := 0;

               if ChipNode.Attributes.GetNamedItem('addrtype') <> nil then
                 if IsNumber(UTF16ToUTF8(ChipNode.Attributes.GetNamedItem('addrtype').NodeValue)) then
                 begin
                   MainForm.RadioI2C.Checked:= true;
                   Main.CurrentICParam.I2CAddrType := StrToInt(UTF16ToUTF8(ChipNode.Attributes.GetNamedItem('addrtype').NodeValue));
                 end;

               if  ChipNode.Attributes.GetNamedItem('page') <> nil then
               begin
                 if UpCase(UTF16ToUTF8(ChipNode.Attributes.GetNamedItem('page').NodeValue)) = 'SSTB' then
                   Main.CurrentICParam.Page := -1
                 else if UpCase(UTF16ToUTF8(ChipNode.Attributes.GetNamedItem('page').NodeValue)) = 'SSTW' then
                   Main.CurrentICParam.Page := -2
                 else
                   Main.CurrentICParam.Page := StrToInt(UTF16ToUTF8(ChipNode.Attributes.GetNamedItem('page').NodeValue));
               end
               else
                 Main.CurrentICParam.Page := 0;

               if ChipNode.Attributes.GetNamedItem('size') <> nil then
                 Main.CurrentICParam.Size:= StrToInt(UTF16ToUTF8(ChipNode.Attributes.GetNamedItem('size').NodeValue))
               else
                 Main.CurrentICParam.Size := 0;

               if ChipNode.Attributes.GetNamedItem('script') <> nil then
               begin
                 Main.CurrentICParam.Script:= UTF16ToUTF8(ChipNode.Attributes.GetNamedItem('script').NodeValue);
                 MainForm.ComboBox_chip_scriptrun.Items := scriptsfunc.GetScriptSectionsFromFile(Main.CurrentICParam.Script);
                 MainForm.ComboBox_chip_scriptrun.ItemIndex := 0;
               end
               else
               begin
                 Main.CurrentICParam.Script := '';
                 MainForm.ComboBox_chip_scriptrun.Items.Clear;
               end;

                MainForm.LabelChipName.Caption := CurrentICParam.Name;

                if CurrentICParam.MWAddLen > 0 then
                  MainForm.ComboMWBitLen.Text := IntToStr(CurrentICParam.MWAddLen)
                else
                  MainForm.ComboMWBitLen.Text := 'MW addr len';

                MainForm.ComboAddrType.ItemIndex := CurrentICParam.I2CAddrType;

                if CurrentICParam.Page > 0 then
                  MainForm.ComboPageSize.Text := IntToStr(CurrentICParam.Page)
                else if CurrentICParam.Page = -1 then
                  MainForm.ComboPageSize.Text := 'SSTB'
                else if CurrentICParam.Page = -2 then
                  MainForm.ComboPageSize.Text := 'SSTW'
                else
                  MainForm.ComboPageSize.Text := 'Page size';

                if CurrentICParam.Size > 0 then
                  MainForm.ComboChipSize.Text := IntToStr(CurrentICParam.Size)
                else
                  MainForm.ComboChipSize.Text := 'Chip size';

              end;
           end;
         end;

       end;
     finally
       Free;
     end;
     Node := Node.NextSibling;
    end;
  end;
end;

{ TChipSearchForm }

procedure TChipSearchForm.EditSearchChange(Sender: TObject);
begin
  ListBoxChips.Clear;
  FindChip(chiplistfile, EditSearch.Text);
end;

procedure TChipSearchForm.ChipSearchSelectButtonClick(Sender: TObject);
begin
  ChipSearchForm.ListBoxChipsDblClick(Sender);
  ChipSearchForm.Hide;
end;

procedure TChipSearchForm.ListBoxChipsDblClick(Sender: TObject);
var
  chipname: string;
begin
  if ListBoxChips.ItemIndex >= 0 then
  begin
    chipname := ListBoxChips.Items[ListBoxChips.ItemIndex];
    chipname := copy(chipname, 1, pos(' (', chipname)-1); //отрезаем фирму
    SelectChip(chiplistfile, chipname);
  end;
end;

end.

