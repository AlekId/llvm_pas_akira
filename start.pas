unit start;

{$ifdef FPC}
{$mode delphi}
{$H+}{$J-}
{$endif}

interface

uses Classes, SysUtils, ast, cntx, parser;

procedure StartCompile;

implementation

uses llvm_codegen, dump;

function ExecProcess(const Cmd: string): integer;
begin
  Result := ExecuteProcess(Cmd, '');
end;

type
  EOptionError = class(Exception);

  TCompiler = class
  private
    FTools_LL2BC, FTools_LL2Obj, FTools_LL2Asm, FTools_BC2Asm, FTools_Link: string;
    FDump, FDumpCode: boolean;
    FEmitLLVM: boolean;
    FParseOnly: boolean;
    FCompileToObj: boolean;
    FCompileToAsm: boolean;
    FIsSystemUnit: boolean;
    FOptLevel: 0..3;
    FOptCPU: String;
    FSource: string;
    FLibDirs, FIncDirs: TStringList;
    FUnitOutputDir: string;
    FExeOutputDir: string;
    FLLVMTarget: string;
    FLLCodeFile: string;
    procedure OnError(ErrInfo: TParserErrorInfo);
    procedure LoadConfig;
    procedure LoadCommandOptions;
    procedure ProcessOptions(Opts: TStringList);
    procedure ShowHelp;
    procedure DoCompile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

procedure StartCompile;
var
  Compiler: TCompiler;
begin
  Compiler := TCompiler.Create;
  Compiler.Run;
  Compiler.Free;
end;

constructor TCompiler.Create;
begin
  FLibDirs := TStringList.Create;
  FIncDirs := TStringList.Create;
  //note that what FPC calls '386' is more often called '686' nowadays
  {$ifdef CPU386}
  {$ifdef WINDOWS}
  FLLVMTarget := 'i686-pc-windows-gnu';
  {$else}
  FLLVMTarget := 'i686-unknown-linux-gnu';
  {$endif}
  {$endif}
  {$ifdef CPU64}
  {$ifdef WINDOWS}
  FLLVMTarget := 'x86_64-pc-windows-gnu';
  {$else}
  FLLVMTarget := 'x86_64-unknown-linux-gnu';
  {$endif}
  {$endif}
end;

destructor TCompiler.Destroy;
begin
  FLibDirs.Free;
  FIncDirs.Free;
  inherited Destroy;
end;

procedure TCompiler.DoCompile;
var
  Context: TCompileContext;
  {LibDir: string;}
  M: TModule;

  procedure DoGenLLCode(M: TModule);
  var
    cg: TCodeGen;
    code: string;
    fs: TFileStream;
  begin
    cg := TCodeGen.Create(Context);
    try
      cg.LLVMTarget := FLLVMTarget;
      cg.EmitModuleDecl(M);
      if Self.FUnitOutputDir = '' then
        FLLCodeFile := ChangeFileExt(FSource, '.ll')
      else
      begin
        FLLCodeFile := FUnitOutputDir + ExtractFileName(FSource);
        FLLCodeFile := ChangeFileExt(FSource, '.ll');
      end;
      code := cg.GetIR;
      fs := TFileStream.Create(FLLCodeFile, fmCreate);
      try
        fs.WriteBuffer(pAnsiChar(code)^, Length(code));
      finally
        fs.Free;
      end;
      M.Codes := code;
    finally
      cg.Free;
    end;
  end;

  function GetCmd(const templ, input, outext: string): string;
  var
    cmd: string;
    infn, outfn, opt: string;
  begin
    cmd := templ;
    infn := '"' + input + '"';
    outfn := '"' + ChangeFileExt(input, outext) + '"';
    opt := IntToStr(FOptLevel);
    if FOptCPU <> '' then
      cmd += (' ' + FOptCPU + ' ');
    cmd := StringReplace(cmd, '%%input', infn, [rfReplaceAll]);
    cmd := StringReplace(cmd, '%%output', outfn, [rfReplaceAll]);
    cmd := StringReplace(cmd, '%%opt', opt, []);
    Result := cmd;
  end;

  function LLToObj: integer;
  begin
    if Self.FEmitLLVM then
      Result := ExecProcess(GetCmd(FTools_LL2BC, FLLCodeFile, '.bc'))
    else
      Result := ExecProcess(GetCmd(FTools_LL2Obj, FLLCodeFile, '.o'));
  end;

  function LLToAsm: integer;
  begin
    if not Self.FEmitLLVM then
      Result := ExecProcess(GetCmd(FTools_LL2Asm, FLLCodeFile, '.s'));
  end;

  procedure Link;
  var
    objfn, outfn, path, sysobj: string;
  begin
    objfn := '"' + ChangeFileExt(FLLCodeFile, '.o') + '"';
    if FExeOutputDir = '' then
      path := ExtractFilePath(FSource)
    else
      path := FExeOutputDir;
    {$ifdef Windows}
    outfn := '"' + path + ChangeFileExt(ExtractFileName(FSource), '.exe') + '"';
    {$else}
    outfn := '"' + path + ChangeFileExt(ExtractFileName(FSource), '') + '"';
    {$endif}
    sysobj := 'system.o';
    ExecProcess(FTools_Link + ' ' + objfn + ' ' + sysobj + ' -o ' + outfn);
  end;

  procedure DoDump(M: TModule);
  var
    d: TDump;
    s: string;
  begin
    d := TJsonDump.Create;
    try
      s := d.Dump(M, DumpAllOptions);
      Writeln(s);
    finally
      d.Free;
    end;
  end;

  procedure DoDumpCode(M: TModule);
  begin
    Writeln(M.Codes);
    Writeln(M.Dump);
  end;

var
  ok: boolean;
begin
  WriteLn('Compile ' + ExtractFileName(FSource) + ' ...');
  try
    System.ExitCode := 1;
    Context := TCompileContext.Create;
    try
      if FLibDirs.Count = 0 then
      begin
        FLibDirs.Add(GetCurrentDIr);
      end;
      Context.LibDirs.Assign(FLibDirs);
      Context.IncDirs.Assign(FIncDirs);
      Context.UnitOutputDir := FUnitOutputDir;
      Context.IsSystemUnit := FIsSystemUnit;
      Context.OnError := OnError;
      M := Context.Compile(FSource);
      if not FParseOnly and not Context.HasError then
      begin
        DoGenLLCode(M);
        if not Self.FCompileToAsm then
          ok := LLToObj = 0
        else
          ok := LLToAsm = 0;
        if ok and not FCompileToAsm and not FCompileToObj and (M.Kind = mkProgram) then
        begin
          Link;
        end;
      end;
      if FDump then
      begin
        DoDump(M);
      end;
      if FDumpCode then
      begin
        DoDumpCode(M);
      end;
    finally
      Context.Free;
    end;
    System.ExitCode := 0;
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      DumpExceptionBackTrace(stdout);
    end;
  end;
end;

procedure TCompiler.LoadCommandOptions;
var
  Count, I: integer;
  Opts: TStringList;
  S, fn: string;
begin
  fn := '';
  Opts := TStringList.Create;
  try
    Count := ParamCount;
    for I := 1 to Count do
    begin
      S := Trim(ParamStr(I));
      if S = '' then
        Continue;
      if S[1] = '-' then
        Opts.Add(S)
      else
      if fn = '' then
        fn := S
      else
        raise EOptionError.Create('Only one source file supported');
    end;
    if fn = '' then
      raise EOptionError.Create('source file required');
    Self.FSource := fn;
    ProcessOptions(Opts);
  finally
    Opts.Free;
  end;
end;

procedure TCompiler.LoadConfig;
const
  WhiteSpaceChars = [#9, #32];

  procedure ParseLine(const line: string; List: TStringList);

    procedure Add(const s: string);
    var
      n: string;
      i: integer;
    begin
      n := s;
      i := Length(n);
      while
        i > 0 do
      begin
        if n[i] = '"' then
        begin
          Delete(n, i, 1);
          if (i > 1) and (n[i - 1] = '"') then
            Dec(i);
        end;
        Dec(i);
      end;
      List.Add(n);
    end;

  var
    i, p1, len: integer;
    s: string;
    inDblQuot: boolean;
  begin
    p1 := 1;
    inDblQuot := False;
    i := 1;
    len := Length(line);
    while i <= len do
    begin
      if (line[i] in WhiteSpaceChars) and not inDblQuot then
      begin
        s := Copy(line, p1, i - p1);
        Add(s);
        while line[i] in WhiteSpaceChars do
          Inc(i);
        p1 := i;
        Continue;
      end
      else if line[i] = '"' then
      begin
        if not inDblQuot then
          inDblQuot := True
        else if ((i + 1) >= len) or (line[i + 1] <> '"') then
          inDblQuot := False
        else
          Inc(i);
      end;
      Inc(i);
    end;
    if p1 < i then
    begin
      s := Copy(line, p1, i - p1);
      Add(s);
    end;
    if inDblQuot then
      raise Exception.Create('double quots mismatch');
  end;

var
  cfgFn, line: string;
  lineno: integer;
  cfgLines, Opts: TStringList;
begin
  cfgFn := GetCurrentDir + DirectorySeparator + 'lpc.cfg';
  if not FileExists(cfgFn) then
    Exit;
  cfgLines := TStringList.Create;
  Opts := TStringList.Create;
  try
    cfgLines.LoadFromFile(cfgFn);
    for lineno := 0 to cfgLines.Count - 1 do
    begin
      line := Trim(cfgLines[lineno]);
      if (line = '') or (line[1] = '#') then
        Continue;
      try
        ParseLine(line, Opts);
      except
        on E: Exception do
        begin
          WriteLn(Format('line %d: %s', [lineno + 1, E.Message]));
          Break;
        end;
      end;
    end;
    ProcessOptions(Opts);
  finally
    Opts.Free;
    cfgLines.Free;
  end;
end;

procedure TCompiler.OnError(ErrInfo: TParserErrorInfo);
const
  Levels: array[TErrorLevel] of string = ('Error', 'Warning', 'Hint');
var
  Msg: string;
begin
  Msg := Format('%s %s(%d,%d): %s', [Levels[ErrInfo.ErrorLevel], ExtractFileName(ErrInfo.FileName),
    ErrInfo.Row, ErrInfo.Column, ErrInfo.ErrorMessage]);
  WriteLn(Msg);
end;

procedure TCompiler.ProcessOptions(Opts: TStringList);

  procedure CheckTools(const s: string; i: integer);
  begin
    if i >= Opts.Count then
      raise EOptionError.CreateFmt('%s option require a command', [s]);
  end;

  function StartWith(const s, sub: string): boolean;
  begin
    if sub <> '' then
    begin
      Result := Copy(s, 1, Length(sub)) = sub;
    end
    else
      Result := True;
  end;

  function CheckPathOption(const s, opt: string; var path: string): boolean;
  begin
    if StartWith(s, opt) then
    begin
      path := Copy(s, Length(opt) + 1, MaxInt);
      Result := True;
    end
    else
      Result := False;
  end;

var
  i: integer;
  s, fn: string;
begin
  fn := '';
  i := 0;
  while i < Opts.Count do
  begin
    s := Opts[i];
    if s = '-c' then
      Self.FCompileToObj := True
    else if s = '-S' then
      Self.FCompileToAsm := True
    else if s = '-Sys' then
      Self.FIsSystemUnit := True
    else if s = '-E' then
      Self.FParseOnly := True
    else if s = '-dump' then
      Self.FDump := True
    else if s = '-dump-code' then
      Self.FDumpCode := True
    else if s = '-O0' then
      Self.FOptLevel := 0
    else
    if s = '-O1' then
      Self.FOptLevel := 1
    else if s = '-O2' then
      Self.FOptLevel := 2
    else if s = '-O3' then
      Self.FOptLevel := 3
    else if S.Contains('-march=') then
      Self.FOptCPU := S
    else if s = '-emit-llvm' then
      Self.FEmitLLVM := True
    else if s = '-sys-unit' then
      Self.FIsSystemUnit := True
    else if CheckPathOption(s, '-Fi', fn) then
    begin
      FIncDirs.Add(IncludeTrailingPathDelimiter(ExpandFileName(fn)));
    end
    else if CheckPathOption(s, '-Fl', fn) then
    begin
      FLibDirs.
        Add(IncludeTrailingPathDelimiter(ExpandFileName(fn)));
    end
    else if CheckPathOption(s, '-FU', fn) then
    begin
      FUnitOutputDir := IncludeTrailingPathDelimiter(ExpandFileName(fn));
    end
    else if CheckPathOption(s, '-FE', fn) then
    begin
      FExeOutputDir := IncludeTrailingPathDelimiter(ExpandFileName(fn));
    end
    else if CheckPathOption(s, '-tools-ll2bc', fn) then
    begin
      FTools_LL2BC := (fn);
    end
    else if CheckPathOption(s, '-tools-ll2obj', fn) then
    begin
      FTools_LL2Obj :=
        (fn);
    end
    else if CheckPathOption(s, '-tools-ll2asm', fn) then
    begin
      FTools_LL2Asm := (fn);
    end
    else if CheckPathOption(s, '-tools-bc2asm', fn) then
    begin
      FTools_BC2Asm := (fn);
    end
    else if CheckPathOption(s, '-tools-link', fn) then
    begin
      FTools_Link := (fn);
    end
    else if CheckPathOption(s, '-llvm-target', fn) then
    begin
      FLLVMTarget := fn;
    end
    else
      raise
      EOptionError.CreateFmt('invalid option %s', [s]);
    Inc(i);
  end;
end;

procedure TCompiler.Run;
begin
  WriteLn('Light Pascal Compiler');
  if ParamCount < 1 then
  begin
    ShowHelp;
    Exit;
  end;
  System.ExitCode := 1;
  try
    LoadConfig
  except
    on E: Exception do
    begin
      WriteLn(Format('config file load failed: %s', [E.Message]));
      Exit;
    end;
  end;
  try
    LoadCommandOptions;
  except
    on E: EOptionError do
    begin
      WriteLn(E.Message);
      Exit;
    end;
  end;
  DoCompile;
end;

procedure TCompiler.ShowHelp;
begin
  WriteLn('Usage: lpc [options] filename [options]');
  WriteLn('Options:');
  WriteLn(' -E           Compile source, no .o or .s file generated');
  WriteLn(' -c           Compile source, generate object .o file');
  WriteLn(' -S           Compile source, generate asm .s file');
  WriteLn(' -Sys         Compile a system unit');
  WriteLn(' -O<n>        Optimization level. n: 0-3');
  WriteLn(' -march=<cpu> CPU to optimize for');
  WriteLn(' -dump        Dump AST nodes');
  WriteLn(' -dump-code   Dump IL code to stdout');
  WriteLn(' -emit-llvm   Keep .ll file');
  WriteLn(' -llvm-target llvm IL code''s target');
end;

end.
