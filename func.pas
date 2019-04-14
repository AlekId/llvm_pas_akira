unit func;

{$ifdef FPC}
{$mode delphi}
{$H+}{$J-}
{$endif}

interface

uses Classes, SysUtils, ast, parser;

procedure CheckFunction(Parser: TParser; Func: TFunction);

implementation

type
  TVisitorProc = procedure(Stmt: TStatement; Data: Pointer);

procedure ForEach(Stmt: TStatement; Proc: TVisitorProc; Data: Pointer);
var
  i: integer;
begin
  if Stmt = nil then
    Exit;
  Proc(Stmt, Data);
  case Stmt.StmtKind of
    skCompoundStmt: for i := 0 to TCompoundStmt(Stmt).Statements.Count - 1 do
        ForEach(TStatement(TCompoundStmt(Stmt).Statements[i]), Proc, Data);
    skIfStmt:
    begin
      ForEach(TIfStmt(Stmt).TrueStmt, Proc, Data);
      ForEach(TIfStmt(Stmt).FalseStmt, Proc, Data);
    end;
    skCaseStmt: for i := 0 to TCaseStmt(Stmt).Count - 1 do
        ForEach(TCaseStmt(Stmt).Selectors[i].Stmt, Proc, Data);
    skForStmt: ForEach(TForStmt(Stmt).Stmt, Proc, Data);
    skWhileStmt: ForEach(TWhileStmt(Stmt).Stmt, Proc, Data);
    skRepeatStmt: ForEach(TRepeatStmt(Stmt).Stmt, Proc, Data);
    skTryStmt:
    begin
      ForEach(TTryStmt(Stmt).Stmt, Proc, Data);
      ForEach(TTryStmt(Stmt).FinallyStmt, Proc, Data);
      if TTryStmt(Stmt).ExceptBlock <> nil then
      begin
        for i := 0 to TTryStmt(Stmt).ExceptBlock.Count - 1 do
          ForEach(TTryStmt(Stmt).ExceptBlock.ExceptHandlers[i].Stmt, Proc, Data);
        ForEach(TTryStmt(Stmt).ExceptBlock.Default, Proc, Data);
      end;
    end;
  end;
end;

function GetUpward(Stmt: TStatement; out Index: integer): TStatement;
var
  Parent, Child: TStatement;
begin
  Parent := Stmt.Parent;
  Child := Stmt;
  while (Parent <> nil) and (Parent.StmtKind <> skCompoundStmt) do
  begin
    Child := Parent;
    Parent := Parent.Parent;
  end;
  if parent = nil then
  begin
    Result := nil;
    Exit;
  end;
  Index := TCompoundStmt(Parent).Statements.IndexOf(Child);
  Result := Parent;
  Assert(Index >= 0, 'StartIndex < 0');
end;

type
  TVarList = array of TSymbol;
  TVarStates = array of boolean;

  TPathInfo = class
  public
    GotoStmt: TGotoStmt;
    Stmt: TCompoundStmt;
    LabStmt: TLabeledStmt;
    StartIndex: integer;
    States: TVarStates;
  end;

  TMsgInfo = class
  public
    Node: TAstNode;
    Coord: TAstNodeCoord;
  end;

  TCheckData = class
  private
    FParser: TParser;
    FFunc: TFunction;
    FVarList: TVarList;
    FVarCount: integer;
    FStates, FKeepStates: array of TVarStates;
    FStateCount, FKeepStateCount: integer;
    FStmtStack: TFPList;
    FPaths: TFPList;
    FFinallyBlocks: TFPList;
    FNoinitVars: TFPList;
    FNoassignVars: TFPList;
    FErrGotos: TFPList;
    FStmt: TStatement;
    FStartIndex: integer;
    FUpward: boolean;
    FCurLoopStmt: TStatement;
    FResultVar: TSymbol;
    FReturnVars: array of TSymbol;
    FReturnVarCount: integer;
    procedure AddPath(GotoStmt: TGotoStmt; States: TVarStates);
    procedure AddVar(Sym: TSymbol);
    procedure AddNoinit(Sym: TSymbol);
    procedure AddNoassign(Sym: TSymbol);
    procedure AddErrGoto(S: TGotoStmt);
    procedure AddReturnVar(Sym: TSymbol);
    procedure PushStmt(AStmt: TStatement);
    procedure PopStmt;
    procedure PushStates;
    procedure PopStates;
    procedure KeepStates(const s: TVarStates);
    function IsInit(Sym: TSymbol): boolean;
    procedure SetInit(Sym: TSymbol; Value: boolean);
    function ExistsInStack(Stmt: TStatement): boolean;
    function LastStates: TVarStates;
    procedure Prepare;
    procedure CheckAssignStmt(Stmt: TAssignmentStmt);
    procedure CheckTryStmt(Stmt: TTryStmt);
    procedure CheckGotoStmt(Stmt: TGotoStmt);
    procedure CheckCompoundStmt(Stmt: TCompoundStmt);
    procedure CheckRepeatStmt(Stmt: TRepeatStmt);
    procedure CheckWhileStmt(Stmt: TWhileStmt);
    procedure CheckForStmt(Stmt: TForStmt);
    procedure CheckIfStmt(Stmt: TIfStmt);
    procedure CheckCaseStmt(Stmt: TCaseStmt);
    procedure CheckCallStmt(Stmt: TCallStmt);
    procedure CheckLabelStmt(Stmt: TLabeledStmt);
    procedure CheckStmt(Stmt: TStatement);
    procedure CheckVarRef(E: TExpr);
    procedure CheckReturnVars;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckMain;
  end;

type
  TIncludeResult = (irSubset = 0, irEquals = 1, irSuperset = 2, irOther = 3);

function StateInclude(S1, S2: TVarStates): TIncludeResult;
var
  i: integer;
begin
  Assert(High(S1) = High(S2), 'StateComp');
  Result := irEquals;
  for i := Low(S1) to High(S1) do
    if S1[i] < S2[i] then
    begin
      if Result = irSuperset then
      begin
        Result := irOther;
        Exit;
      end;
      Result := irSubset;
    end
    else if S1[i] > S2[i] then
    begin
      if Result = irSubset then
      begin
        Result := irOther;
        Exit;
      end;
      Result := irSuperset;
    end;
end;

procedure StateUnion(S1, S2: TVarStates);
var
  i: integer;
begin
  for i := Low(S1) to High(S2) do
    S1[i] := S1[i] or S2[i];
end;

procedure TCheckData.AddErrGoto(S: TGotoStmt);
begin
  if FErrGotos = nil then
    FErrGotos := TFPList.Create;
  if FErrGotos.IndexOf(S) < 0 then
    FErrGotos.Add(S);
end;

procedure TCheckData.AddNoassign(Sym: TSymbol);
begin
  if FNoassignVars = nil then
    FNoassignVars := TFPList.Create;
  if FNoassignVars.IndexOf(Sym) < 0 then
    FNoassignVars.Add(Sym);
end;

procedure TCheckData.AddNoinit(Sym: TSymbol);
begin
  if FNoinitVars = nil then
    FNoinitVars := TFPList.Create;
  if FNoinitVars.IndexOf(Sym) < 0 then
    FNoinitVars.Add(Sym);
end;

procedure TCheckData.AddPath(GotoStmt: TGotoStmt; States: TVarStates);

  function FindPath(S: TStatement; StartIndex: integer; States: TVarStates): boolean;
  var
    i: integer;
    Path: TPathInfo;
  begin
    for i := 0 to
      FPaths.Count - 1 do
    begin
      Path := TPathInfo(FPaths[i]);
      if (Path.Stmt = S) and (Path.StartIndex <= Path.StartIndex) and
        (StateInclude(Path.States, States) in [irSubset, irEquals]) then
      begin
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;

var
  Path: TPathInfo;
  UpStmt: TStatement;
  Index: integer;
begin
  if GotoStmt.StmtLabel.Stmt = nil then
    Exit;
  UpStmt := GetUpward(GotoStmt.StmtLabel.Stmt, Index);
  if FindPath(UpStmt, Index, States) then
    Exit;
  Path := TPathInfo.Create;
  Path.GotoStmt := GotoStmt;
  Path.LabStmt := GotoStmt.StmtLabel.Stmt;
  Path.Stmt := TCompoundStmt(UpStmt);
  Path.StartIndex := Index;
  Path.States := States;
  FPaths.Add(Path);
end;

procedure TCheckData.AddReturnVar(Sym: TSymbol);
begin
  if FReturnVarCount = Length(FReturnVars) then
    SetLength(FReturnVars, FReturnVarCount + 4);
  FReturnVars[FReturnVarCount] := Sym;
  Inc(FReturnVarCount);
end;

procedure TCheckData.AddVar(Sym: TSymbol);
begin
  if FVarCount = Length(FVarList) then
    SetLength(FVarList, FVarCount + 10);
  FVarList[FVarCount] := Sym;
  Inc(FVarCount);
end;

procedure TCheckData.CheckAssignStmt(Stmt: TAssignmentStmt);

  function GetRef(E: TExpr): TSymbol;
  begin
    while E.OpCode = opCAST do
    begin
      E := TBinaryExpr(E).Right;
    end;
    if E <> nil then
      Result := E.GetReference
    else
      Result := nil;
  end;

var
  Sym: TSymbol;
begin
  Sym := GetRef(Stmt.Left);
  if Sym <> nil then
  begin
    CheckVarRef(Stmt.Right);
    SetInit(Sym, True);
  end
  else
  begin
    CheckVarRef(Stmt.Right);
    CheckVarRef(Stmt.Left);
  end;
end;

procedure TCheckData.CheckCallStmt(Stmt: TCallStmt);

  procedure CheckFinallyBlock(TopStmt: TStatement);

    function IsParentTopStmt(T: TStatement): boolean;
    begin
      while T <> nil do
      begin
        if T = TopStmt then
        begin
          Result := True;
          Exit;
        end;
        T := T.Parent;
      end;
      Result := False;
    end;

  var
    i: integer;
    LStmt: TTryStmt;
  begin
    for i := FFinallyBlocks.Count - 1 downto 0 do
    begin
      LStmt := TTryStmt(FFinallyBlocks[i]);
      assert(LStmt.FinallyStmt <> nil, 'CheckFinallyBlock');
      if (TopStmt <> nil) and not IsParentTopStmt(LStmt) then
        Break;
      CheckStmt(LStmt.FinallyStmt);
    end;
  end;

  function CheckContinueBreakExit(CallStmt: TCallStmt): boolean;
  var
    Ref: TSymbol;
  begin
    Result := False;
    Ref := TBinaryExpr(CallStmt.CallExpr).Left.GetReference;
    if Ref <> nil then
    begin
      if Ref.NodeKind = nkBuiltinFunc then
      begin
        case TBuiltinFunction(Ref).Kind of
          bfContinue, bfBreak:
          begin
            assert(FCurLoopStmt <> nil, 'CheckContinueBreakExit');
            CheckFinallyBlock(FCurLoopStmt);
            if (FCurLoopStmt <> nil) and (FCurLoopStmt.StmtKind = skRepeatStmt) then
              CheckVarRef(TRepeatStmt(FCurLoopStmt).Condition);
            Result := True;
            if TBuiltinFunction(Ref).Kind = bfContinue then
              Include(Stmt.Attr, stmtContinue)
            else
              Include(Stmt.Attr, stmtBreak);
          end;
          bfExit:
          begin
            CheckFinallyBlock(nil);
            with TBinaryExpr(CallStmt.CallExpr) do
              if (Right <> nil) and (TUnaryExpr(Right).Operand <> nil) then
                SetInit(FResultVar, True);
            CheckReturnVars;
            Result := True;
            Include(Stmt.Attr, stmtExit);
          end;
        end;
      end;
    end;
  end;

  procedure CheckArgs(S: TCallStmt);
  var
    i: integer;
    List: TListExpr;
  begin
    List := TListExpr(TBinaryExpr(S.CallExpr).Right);
    for i := 0 to List.Count - 1 do
    begin
      Self.CheckVarRef(List.Items[i]);
    end;
  end;

begin
  CheckContinueBreakExit(Stmt);
  CheckArgs(Stmt);
end;

procedure TCheckData.CheckCaseStmt(Stmt: TCaseStmt);
var
  i, j: integer;
  LSaved1, LSaved2: TVarStates;
begin
  CheckVarRef(Stmt.Expr);
  if Stmt.Default <> nil then
  begin
    PushStates;
    LSaved1 := Self.LastStates;
    CheckStmt(Stmt.Default);
    PopStates;
  end;
  for i := 0 to Stmt.Count - 1 do
  begin
    PushStates;
    LSaved2 := Self.LastStates;
    CheckStmt(Stmt.Selectors[i].Stmt);
    PopStates;
    if Assigned(LSaved1) then
    begin
      for j := Low(LSaved1) to High(LSaved1) do
        LSaved1[j] := LSaved1[j] and LSaved2[j];
    end;
  end;
  if Assigned(LSaved1) then
  begin
    LSaved2 := Self.LastStates;
    for j := Low(LSaved1) to High(LSaved1) do
      LSaved2[j] := LSaved1[j];
  end;
end;

procedure TCheckData.CheckCompoundStmt(Stmt: TCompoundStmt);

  procedure GoUpward;
  var
    UpStmt: TStatement;
    OldIndex, Index: integer;
  begin
    UpStmt := GetUpward(Stmt, Index);
    if UpStmt <> nil then
    begin
      PushStates;
      OldIndex := Index;
      FStartIndex := Index;
      CheckStmt(UpStmt);
      PopStates;
      FStartIndex := OldIndex;
    end;
  end;

  procedure SetUnreachable(Stmt: TCompoundStmt; Offset: integer);
  var
    i: integer;
    LCurStmt: TStatement;
  begin
    for i := Offset to Stmt.Statements.Count - 1 do
    begin
      LCurStmt := TStatement(Stmt.Statements[i]);
      if LCurStmt.StmtKind = skLabelStmt then
        Break;
      Include(LCurStmt.Attr, stmtUnreachable);
    end;
  end;

var
  i: integer;
  LCurStmt: TStatement;
begin
  for
    i := FStartIndex to Stmt.Statements.Count - 1 do
  begin
    LCurStmt := TStatement(Stmt.Statements[i]);
    CheckStmt(LCurStmt);
    if stmtNoreturn in LCurStmt.Attr then
    begin
      Include(Stmt.Attr, stmtNoreturn);
      SetUnreachable(Stmt, i + 1);
      Break;
    end;
    case LCurStmt.StmtKind of
      skGotoStmt, skRaiseStmt: SetUnreachable(Stmt, i + 1);
      skCallStmt: if (stmtBreak in LCurStmt.Attr) or (stmtContinue in LCurStmt.Attr) or
          (stmtExit in LCurStmt.Attr) then
        begin
          SetUnreachable(Stmt, i + 1);
        end;
    end;
  end;
  if FUpward then
  begin
    GoUpward;
  end;
end;

procedure TCheckData.CheckForStmt(Stmt: TForStmt);
var
  OldStmt: TStatement;
begin
  CheckVarRef(Stmt.Start);
  CheckVarRef(Stmt.Stop);
  SetInit(Stmt.Value, True);
  if Stmt.Stmt <> nil then
  begin
    OldStmt := FCurLoopStmt;
    FCurLoopStmt := Stmt;
    CheckStmt(Stmt.Stmt);
    FCurLoopStmt := OldStmt;
  end;
end;

procedure TCheckData.CheckGotoStmt(Stmt: TGotoStmt);

  function GetOuterStmt(S: TStatement): TStatement;
  begin
    while S.Parent <> nil do
    begin
      if S.Parent.StmtKind = skTryStmt then
      begin
        Result := S;
        Exit;
      end;
      S := S.Parent;
    end;
    Result := nil;
  end;

begin
  Assert(Stmt.StmtLabel <> nil, 'CheckGotoStmt');
  Self.AddPath(Stmt, Self.LastStates);
  if Stmt.StmtLabel.Stmt <> nil then
  begin
    if GetOuterStmt(Stmt.StmtLabel.Stmt) <> GetOuterStmt(Stmt) then
      AddErrGoto(Stmt);
  end
  else
    AddErrGoto(Stmt);
end;

procedure TCheckData.CheckIfStmt(Stmt: TIfStmt);
var
  LSaved1, LSaved2, LCur: TVarStates;
  i: integer;
begin
  CheckVarRef(Stmt.Value);
  PushStates;
  LSaved1 := LastStates;
  CheckStmt(Stmt.TrueStmt);
  PopStates;
  if Stmt.FalseStmt <> nil then
  begin
    PushStates;
    LSaved2 := LastStates;
    CheckStmt(Stmt.FalseStmt);
    PopStates;
    LCur := LastStates;
    for i := 0 to High(LSaved2) do
      LCur[i] := LSaved1[i] and LSaved2[i];
    if (stmtNoreturn in Stmt.FalseStmt.Attr) and (stmtNoreturn in Stmt.TrueStmt.Attr) then
      Include(Stmt.Attr, stmtNoreturn);
  end;
end;

procedure TCheckData.CheckLabelStmt(Stmt: TLabeledStmt);
var
  LStat: TVarStates;
begin
  LStat := Self.LastStates;
  KeepStates(LStat);
  if Stmt.Data = nil then
    Stmt.Data := Pointer(LStat)
  else if StateInclude(Stmt.Data, LStat) = irSuperset then
    Stmt.Data := Pointer(LStat);
end;

procedure TCheckData.CheckMain;
var
  i: integer;
  Path: TPathInfo;
  sym: TSymbol;
begin
  PushStates;
  CheckStmt(FFunc.StartStmt);
  CheckReturnVars;
  PopStates;
  if stmtNoreturn in FFunc.StartStmt.Attr then
    Include(FFunc.Modifiers, fmNoReturn);
  i := 0;
  while i < FPaths.Count do
  begin
    Path := TPathInfo(FPaths[i]);
    if StateInclude(Path.States, TVarStates(Path.LabStmt.Data)) in [irSubset, irEquals] then
    begin
      PushStates;
      FStates[Self.FStateCount - 1] := Path.States;
      Self.FStartIndex := Path.StartIndex;
      Self.FUpward := True;
      CheckStmt(Path.Stmt);
      PopStates;
    end;
    Inc(i);
  end;
  if Self.FNoinitVars <> nil then
  begin
    for i := 0 to Self.FNoinitVars.Count - 1 do
    begin
      Sym := TSymbol(FNoinitVars[i]);
      FParser.DoWarning(Sym.Coord, '%s might not have been initialized', [Sym.Name]);
    end;
  end;
  if Self.FErrGotos <> nil then
  begin
    for i := 0 to Self.FNoinitVars.Count - 1 do
    begin
      with TGotoStmt(FNoinitVars[i]) do
        if StmtLabel.Stmt = nil then
          FParser.ParseError(Coord, 'Label declared and referenced, but not set: %s', [StmtLabel.Name])
        else
          FParser.ParseError(Coord, 'GOTO leads into or out of TRY statement');
    end;
  end;
  if (Self.FNoassignVars <> nil) and not (stmtNoreturn in FFunc.StartStmt.Attr) then
  begin
    for i := 0 to Self.FNoassignVars.Count - 1 do
    begin
      with TSymbol(Self.FNoassignVars[i]) do
        FParser.DoWarning(Coord, '%s might be undefined', [Name]);
    end;
  end;
end;

procedure TCheckData.CheckRepeatStmt(Stmt: TRepeatStmt);
var
  OldStmt: TStatement;
begin
  OldStmt := FCurLoopStmt;
  FCurLoopStmt := Stmt;
  CheckStmt(Stmt.Stmt);
  FCurLoopStmt := OldStmt;
  CheckVarRef(Stmt.Condition);
end;

procedure TCheckData.CheckReturnVars;
var
  i: integer;
begin
  for i := 0 to FReturnVarCount - 1 do
  begin
    if not IsInit(FReturnVars[i]) then
      AddNoassign(FReturnVars[i]);
  end;
end;

procedure TCheckData.CheckStmt(Stmt: TStatement);
begin
  if Stmt = nil then
    Exit;
  PushStmt(Stmt);
  case Stmt.StmtKind of
    skGotoStmt: CheckGotoStmt(TGotoStmt(Stmt));
    skCompoundStmt: CheckCompoundStmt(TCompoundStmt(Stmt));
    skIfStmt:
      CheckIfStmt(TIfStmt(Stmt));
    skCaseStmt: CheckCaseStmt(TCaseStmt(Stmt));
    skWhileStmt:
      CheckWhileStmt(TWhileStmt(Stmt));
    skRepeatStmt: CheckRepeatStmt(TRepeatStmt(Stmt));
    skForStmt: CheckForStmt(TForStmt(Stmt));
    skAssignmentStmt: CheckAssignStmt(TAssignmentStmt(Stmt));
    skTryStmt: CheckTryStmt(TTryStmt(Stmt));
    skLabelStmt: CheckLabelStmt(TLabeledStmt(Stmt));
    skCallStmt: CheckCallStmt(TCallStmt(Stmt));
  end;
  PopStmt;
end;

procedure TCheckData.CheckTryStmt(Stmt: TTryStmt);

  procedure CheckExceptBlock(ExceptBlock: TExceptBlock);
  var
    i: integer;
    Handler: TExceptHandler;
  begin
    for i := 0 to ExceptBlock.Count - 1 do
    begin
      PushStates;
      Handler := ExceptBlock.ExceptHandlers[i];
      CheckStmt(Handler.Stmt);
      PopStates;
    end;
    if ExceptBlock.Default <> nil then
    begin
      PushStates;
      CheckStmt(ExceptBlock.Default);
      PopStates;
    end;
  end;

var
  LSaved1: TVarStates;
begin
  if Stmt.FinallyStmt <> nil then
  begin
    Self.FFinallyBlocks.Add(Stmt);
  end;
  if Stmt.Stmt <> nil then
  begin
    PushStates;
    LSaved1 := Self.LastStates;
    CheckStmt(Stmt.Stmt);
    PopStates;
  end;
  if Stmt.FinallyStmt <> nil then
  begin
    Self.FFinallyBlocks.Delete(FFinallyBlocks.Count - 1);
    CheckStmt(Stmt.FinallyStmt);
    StateUnion(Self.LastStates, LSaved1);
  end;
  if Stmt.ExceptBlock <> nil then
    CheckExceptBlock(Stmt.ExceptBlock);
end;

procedure TCheckData.CheckVarRef(E: TExpr);

  procedure CheckVar(Sym: TSymbol);
  begin
    if Assigned(Sym) and (Sym.NodeKind in [nkVariable, nkFuncParam]) then
      if not IsInit(Sym) then
        AddNoinit(Sym);
  end;

  procedure CheckOutArg(E: TSymbolExpr);
  var
    I: integer;
    List: TListExpr;
    CallExpr: TBinaryExpr;
    F: TFunctionDecl;
    typ: TType;
  begin
    List := TListExpr(E.Parent);
    if (List.Parent = nil) or (List.Parent.OpCode <> opCALL) then
      Exit;
    CallExpr := TBinaryExpr(List.Parent);
    typ := CallExpr.Left.Typ;
    if typ.TypeCode = typProcedural then
    begin
      I := List.IndexOf(E);
      Assert(I >= 0, 'CheckOutArg');
      if I < TProceduralType(typ).ParamCount then
      begin
        if TProceduralType(typ).Params[I].Modifier = argOut then
          Self.SetInit(E.Reference, True);
      end;
    end
    else
    begin
      I := List.IndexOf(E);
      Assert(I >= 0, 'CheckOutArg');
      F := CallExpr.Left.GetFunctionSymbol;
      if (F <> nil) and (I < F.ParamCount) then
      begin
        if F.Params[I].Modifier = argOut then
          Self.SetInit(E.Reference, True);
      end;
    end;
  end;

  procedure CheckList(E: TListExpr);
  var
    I: integer;
  begin
    for I := 0 to E.Count - 1 do
    begin
      CheckVarRef(E.Items[I]);
    end;
  end;

begin
  case OpKinds[E.OpCode] of
    opkUnary:
    begin
      CheckVarRef(TUnaryExpr(E).Operand);
    end;
    opkBinary:
    begin
      CheckVarRef(TBinaryExpr(E).Left);
      CheckVarRef(TBinaryExpr(E).Right);
    end;
    opkList:
    begin
      CheckList(TListExpr(E));
    end;
    else
      if E.OpCode = opSYMBOL then
      begin
        if Assigned(E.Parent) and (E.Parent.OpCode = opLIST) then
          CheckOutArg(TSymbolExpr(E));
        if Assigned(E.Parent) and (E.Parent.OpCode <> opADDR) then
          CheckVar(TSymbolExpr(E).Reference);
      end;
  end;
end;

procedure TCheckData.CheckWhileStmt(Stmt: TWhileStmt);
var
  OldStmt: TStatement;
begin
  CheckVarRef(Stmt.Condition);
  if Stmt.Stmt <> nil then
  begin
    OldStmt := FCurLoopStmt;
    FCurLoopStmt := Stmt;
    PushStates;
    CheckStmt(Stmt.Stmt);
    PopStates;
    FCurLoopStmt := OldStmt;
  end;
end;

constructor TCheckData.Create;
begin
  FStmtStack := TFPList.Create;
  FStmtStack.Capacity := 64;
  FPaths := TFPList.Create;
  FFinallyBlocks := TFPList.Create;
end;

destructor TCheckData.Destroy;
var
  i: integer;
begin
  for i := 0 to FPaths.Count - 1 do
    TObject(FPaths[i]).Free;
  FStmtStack.Free;
  FPaths.Free;
  FFinallyBlocks.Free;
  FNoinitVars.Free;
  FNoassignVars.Free;
  FErrGotos.Free;
  inherited;
end;

function TCheckData.ExistsInStack(Stmt: TStatement): boolean;
var
  i: integer;
begin
  for i := FStmtStack.Count - 2 downto 0 do
    if FStmtStack[i] = Stmt then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

function TCheckData.IsInit(Sym: TSymbol): boolean;
begin
  Result := True;
  if Sym.Parent <> FFunc then
    Exit;
  Assert(FStateCount > 0, 'IsInit');
  case sym.NodeKind of
    nkVariable: if (TVariable(sym).Index = $ffff) then
        Result := True
      else
        Result := FStates[FStateCount - 1][TVariable(sym).Index];
    nkFuncParam: if (TFuncParam(sym).Index = $ffff) then
        Result := True
      else
        Result := FStates[FStateCount - 1][TFuncParam(sym).Index];
  end;
end;

procedure TCheckData.KeepStates(const s: TVarStates);
begin
  if FKeepStateCount = Length(FKeepStates) then
    SetLength(FKeepStates, FKeepStateCount + 10);
  FKeepStates[FKeepStateCount] := S;
  Inc(FKeepStateCount);
end;

function TCheckData.LastStates: TVarStates;
begin
  Assert(FStateCount > 0, 'LastStates');
  Result :=
    FStates[FStateCount - 1];
end;

procedure TCheckData.PopStates;
begin
  Assert(FStateCount > 0, 'PopState');
  FStates[FStateCount - 1] := nil;
  Dec(FStateCount);
end;

procedure TCheckData.PopStmt;
begin
  Assert(FStmtStack.Count > 0, 'PopStmt');
  FStmtStack.Delete(FStmtStack.Count - 1);
  if FStmtStack.Count = 0 then
    FStmt := nil
  else
    FStmt := TStatement(FStmtStack.Last);
end;

procedure TCheckData.Prepare;

  function NeedCheckInit(V: TVariable): boolean;
  begin
    Result := (not (vaSelf in V.VarAttr)) and not (V.VarType.TypeCode in AutoInitTypes) and
      not (V.VarType.TypeCode in [typRecord, typObject, typArray]);
  end;

var
  i, j: integer;
  Sym: TSymbol;
begin
  j := 0;
  for i := 0 to FFunc.LocalSymbols.Count - 1 do
  begin
    Sym := FFunc.LocalSymbols[i];
    case Sym.NodeKind of
      nkFuncParam: if argOut = TFuncParam(Sym).Modifier then
        begin
          AddVar(Sym);
          AddReturnVar(Sym);
          TFuncParam(Sym).Index := j;
          Inc(j);
        end
        else
          TFuncParam(Sym).Index := $ffff;
      nkVariable: if NeedCheckInit(TVariable(Sym)) then
        begin
          AddVar(Sym);
          TVariable(Sym).Index := j;
          Inc(j);
          if (vaResult in TVariable(Sym).VarAttr) and not ((FFunc.NodeKind = nkMethod) and
            (TMethod(FFunc).MethodKind = mkConstructor)) then
          begin
            FResultVar := Sym;
            AddReturnVar(Sym);
          end;
        end
        else
          TVariable(Sym).Index := $ffff;
    end;
  end;
end;

procedure TCheckData.PushStates;
begin
  if FStateCount = Length(FStates) then
    SetLength(FStates, FStateCount + 10);
  SetLength(FStates[FStateCount], FVarCount);
  if FStateCount = 0 then
    SetLength(FStates[0], FVarCount)
  else
    Move(FStates[FStateCount - 1][0], FStates[FStateCount][0], FVarCount * SizeOf(boolean));
  Inc(FStateCount);
end;

procedure TCheckData.PushStmt(AStmt: TStatement);
begin
  FStmt := AStmt;
  FStmtStack.Add(AStmt);
end;

procedure TCheckData.SetInit(Sym: TSymbol; Value: boolean);
begin
  Assert(FStateCount > 0, 'SetInit');
  case sym.NodeKind of
    nkVariable: if (TVariable(sym).Index < $ffff) and (sym.Parent = FFunc) then
        FStates[FStateCount - 1][TVariable(sym).Index] := Value;
    nkFuncParam: if (TFuncParam(sym).Index < $ffff) and (sym.Parent = FFunc) then
        FStates[FStateCount - 1][TFuncParam(sym).Index] := Value;
  end;
end;

procedure CheckFunction(Parser: TParser; Func: TFunction);
var
  Data: TCheckData;
begin
  Data := TCheckData.Create;
  try
    Data.FParser := Parser;
    Data.FFunc := Func;
    Data.Prepare;
    Data.CheckMain;
  finally
    Data.Free;
  end;
end;

end.
