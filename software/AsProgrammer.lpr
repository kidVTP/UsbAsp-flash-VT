program AsProgrammer;

{$SetPEFlags $20}     // Large Address Aware (cho phép dùng đến 4GB RAM trên Windows 64-bit)
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, i2c, microwire,
  spi95, search, sregedit, findchip, ScriptEdit, spi25, spi25nand;

{$R *.res}

begin
  LoadXML;
  Translate(SettingsFile);
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSearchForm, SearchForm);
  Application.CreateForm(TsregeditForm, sregeditForm);
  Application.CreateForm(TChipSearchForm, ChipSearchForm);
  Application.CreateForm(TScriptEditForm, ScriptEditForm);
  Application.Run;
end.

