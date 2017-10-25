{*
 * WildcardSeeker: A wildcard searching class written on Pascal and optimized for speed.
 * Jonas Raoni Soares da Silva <http://raoni.org>
 * https://github.com/jonasraoni/wildcard-seeker
 *}

unit WildcardSeeker;

interface

uses
  SysUtils, Classes;

type

{
  @code(EWildcardSeekerException) -
    Notificar erros na classe TWildcardSeeker
}
  EWildcardSeekerException = class ( Exception )
  public
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
  end;

  {Opões de pesquisa: <BR>
   @code(nsHandleEOL) - se você precisar buscar por quebras de linhas, você precisa setar esta opção.<BR>
   @code(nsCaseSensitive) - diferenciar maiúsculas de minúsculas.<BR>
   @code(nsWholeWords) - retorna apenas palavras inteiras.<BR>
   @code(nsBackward) - busca de traz para frente. <BR>
   @code(nsHandleWildCard) - usa coringas * e ? na pesquisa.}
  TWildcardSeekerOption = ( nsHandleEOL, nsCaseSensitive, nsWholeWords, nsBackward, nsHandleWildCard );
  { Set de @link(TWildcardSeekerOption).}
  TWildcardSeekerOptions = set of TWildcardSeekerOption;

  TSearchFunction = function: Boolean of object;

{
  @code(TWildcardSeeker) -
    Permite fazer buscas em strings com várias opções
}
  TWildcardSeeker = class(TObject)
  private
    Jump, LineJump: Cardinal;
    FList: TList;
  protected
    FMatches, FStartAt, FEOLLen, FSearchLen, FCurCol,
    FCurLine, FMatchLen, FMatchLine, FMatchCol: Cardinal;

    FBufferEnd, FBuffer, FBufferBegin, FBufferBackup,
    FEOL, FSearchBegin, FSearch, FSearchEnd: PChar;

    FOptions: TWildcardSeekerOptions;

    FContextRightLenght, FContextLeftLenght: Cardinal;

    FKeepText: Boolean;

    function GetText: string;
    function GetReplacedText: string;
    function GetContext: string;
    function GetSearchStr: string;
    function GetRemainingText: string;
    function GetCurByte: Cardinal;
    function GetEOL: string;

    procedure SetOptions(const Value: TWildcardSeekerOptions);
    procedure SetText( const Value: string);
    procedure SetSearchStr(const Value: string);
    procedure SetEOL(const Value: string);

    procedure FreeBuffer;
    procedure FreeEOL;
    procedure FreeSearchStr;

    {Search Engines}
    function SearchForward: Boolean;
    function SearchForwardWithWildCard: Boolean;
    function SearchBackward: Boolean;
    function SearchBackwardWithWildCard: Boolean;

  public
    { Efetua a busca: se o termo procurado for encontrado, retorna true, caso contrário retorna false }
    Search: TSearchFunction;

    { Método construtor }
    constructor Create; virtual;
    { Método destruidor }
    destructor Destroy; override;

    { Armazena o tamanho do "match", quando a opção wildcard estiver desligada esta será igual ao tamanho da própria string procurada }
    property MatchLength: Cardinal read FMatchLen;
    { Quando HandleEOL fizer parte das opções, armazenará a linha onde a string procurada foi encontrada }
    property CurLine: Cardinal read FMatchLine;
    { Armazenará a coluna onde a string procurada foi encontrada, se HandleEOL não estiver nas opções, armazenará a mesma coisa que a propriedade CurByte }
    property CurCol: Cardinal read FMatchCol;
    { Armazena a posição ou byte "absoluto" onde a string foi encontrada }
    property CurByte: Cardinal read GetCurByte;
    { Especifica a posição/byte inicial onde a busca deverá começar }
    property StartAt: Cardinal read FStartAt write FStartAt;
    { Retorna o contexto onde a string procurada foi encontrada }
    property Context: string read GetContext;
    { Especifica a quantidade de caracteres que deverão fazer parte do contexto encontrado ao lado esquerdo da string procurada }
    property ContextLeftLenght: Cardinal read FContextLeftLenght write FContextLeftLenght;
    { Especifica a quantidade de caracteres que deverão fazer parte do contexto encontrado ao lado direito da string procurada }
    property ContextRightLenght: Cardinal read FContextRightLenght write FContextRightLenght;
    { Armazena o número de strings que coincidiram com a busca até o presente momento }
    property Matches: Cardinal read FMatches;
    { Permite alterar a sequência de caracteres que demarcam o fim de uma linha }
    property EOL: string read GetEOL write SetEOL;
    { Armazena as opções atualmente habilitadas para a busca, podendo ser alterada a qualquer momento }
    property Options: TWildcardSeekerOptions read FOptions write SetOptions;
    { Termo a ser procurado no texto }
    property SearchStr: string read GetSearchStr write SetSearchStr;
    { Texto onde a busca será efetuada }
    property Text: string read GetText write SetText;
    { Texto restante ao término da busca }
    property RemainingText: string read GetRemainingText;
    { Especifica se a classe deverá manter uma cópia do texto setado inicialmente }
    property KeepText: Boolean read FKeepText write FKeepText;
    { Retorna o texto com os replaces, caso KeepText seja falso, essa propriedade se torna sinônimo da propriedade Text }
    property ReplacedText: string read GetReplacedText;

    { Prepara tudo para uma nova busca }
    procedure StartSearch;
    { Carrega o texto da busca a partir de um arquivo }
    procedure LoadFromFile( const AFilename: string );
    { Carrega o texto da busca a partir de um stream }
    procedure LoadFromStream( const AStream: TStream );
    { Carrega o texto da busca a partir de um buffer }
    procedure LoadFromBuffer( const ABuffer: PChar );
    { Efetua a substituição da string encontrada pela string contida em "S" }
    procedure Replace( const S: String );
    { Modo prático para setar as opções }
    procedure EnableOptions( const CaseSensitive: Boolean = true; const WholeWords: Boolean = false; const HandleEOL: Boolean = true; const HandleWildCard: Boolean = false; const Backward: Boolean = false );

  end;

  { Compara Str1 e Str2 de trás pra frente, se as duas forem iguais retorna true, caso contrário false }
  function StrLRComp( S1, S2: PChar; const S2Begin: PChar ): Boolean;
  { Converte para maiúsculo (ANSI) -> VALEUUUUU TIO RUSSÃO hahaha, o que tem no delphi "aplica a alteração"
    Idéia de manter tabela com tudo maiúsculo arrancada de "QStrings 6.07.424 Copyright (C) 2000, 2003 Andrew Dryazgov [ andrewdr@newmail.ru ]" }
  function AnsiUpCase(Ch: Char): Char;

const
  { Caracteres que definem delimitadores de palavra, usada quando a opção WholeWords está ativa }
  WhiteSpaces: set of Char = [' ',#9,#13,#10,'!','"','#','$','%','&','''','(',')','*','+','-','/',':',';','<','=','>','?','@','[','\',']','^','`','{','|','}','~'];

const
  //fiz algumas alterações hehe, o tiozaum russo devia tá começano a ficar cego enqto fazia isso :)
  ToUpperChars: array[0..255] of Char =
    (#$00,#$01,#$02,#$03,#$04,#$05,#$06,#$07,#$08,#$09,#$0A,#$0B,#$0C,#$0D,#$0E,#$0F,
     #$10,#$11,#$12,#$13,#$14,#$15,#$16,#$17,#$18,#$19,#$1A,#$1B,#$1C,#$1D,#$1E,#$1F,
     #$20,#$21,#$22,#$23,#$24,#$25,#$26,#$27,#$28,#$29,#$2A,#$2B,#$2C,#$2D,#$2E,#$2F,
     #$30,#$31,#$32,#$33,#$34,#$35,#$36,#$37,#$38,#$39,#$3A,#$3B,#$3C,#$3D,#$3E,#$3F,
     #$40,#$41,#$42,#$43,#$44,#$45,#$46,#$47,#$48,#$49,#$4A,#$4B,#$4C,#$4D,#$4E,#$4F,
     #$50,#$51,#$52,#$53,#$54,#$55,#$56,#$57,#$58,#$59,#$5A,#$5B,#$5C,#$5D,#$5E,#$5F,
     #$60,#$41,#$42,#$43,#$44,#$45,#$46,#$47,#$48,#$49,#$4A,#$4B,#$4C,#$4D,#$4E,#$4F,
     #$50,#$51,#$52,#$53,#$54,#$55,#$56,#$57,#$58,#$59,#$5A,#$7B,#$7C,#$7D,#$7E,#$7F,
     #$80,#$81,#$82,#$81,#$84,#$85,#$86,#$87,#$88,#$89,#$8A,#$8B,#$8C,#$8D,#$8E,#$8F,
     #$90,#$91,#$92,#$93,#$94,#$95,#$96,#$97,#$98,#$99,#$8A,#$9B,#$8C,#$9D,#$9E,#$9F,
     #$A0,#$A1,#$A1,#$A3,#$A4,#$A5,#$A6,#$A7,#$A8,#$A9,#$AA,#$AB,#$AC,#$AD,#$AE,#$AF,
     #$B0,#$B1,#$B2,#$B2,#$A5,#$B5,#$B6,#$B7,#$A8,#$B9,#$BA,#$BB,#$BC,#$BD,#$BE,#$BF,
     #$C0,#$C1,#$C2,#$C3,#$C4,#$C5,#$C6,#$C7,#$C8,#$C9,#$CA,#$CB,#$CC,#$CD,#$CE,#$CF,
     #$D0,#$D1,#$D2,#$D3,#$D4,#$D5,#$D6,#$D7,#$D8,#$D9,#$DA,#$DB,#$DC,#$DD,#$DE,#$DF,
     #$C0,#$C1,#$C2,#$C3,#$C4,#$C5,#$C6,#$C7,#$C8,#$C9,#$CA,#$CB,#$CC,#$CD,#$CE,#$CF,
     #$D0,#$D1,#$D2,#$D3,#$D4,#$D5,#$D6,#$F7,#$D8,#$D9,#$DA,#$DB,#$DC,#$DD,#$DE,#$9F);

implementation

function StrLRComp( S1, S2: PChar; const S2Begin: PChar ): Boolean;
begin
  while ( S2 <> S2Begin ) and ( S1^ = S2^ ) do begin
    dec( S1 );
    dec( S2 );
  end;
  Result := ( S1^ = S2^ ) and ( S2 = S2Begin );
end;

function AnsiUpCase(Ch: Char): Char;
begin
  Result := ToUpperChars[ ord( ch ) ];
end;


{ class : TWildcardSeeker }

{ TWildcardSeeker : protected }

function TWildcardSeeker.GetText: string;
begin
  if Assigned( FBufferBackup ) then
    Result := StrPas( FBufferBackup )
  else
    Result := ReplacedText;
end;

function TWildcardSeeker.GetReplacedText: string;
begin
  Result := StrPas( FBufferBegin );
end;

function TWildcardSeeker.GetContext: string;
var
  BeginAt, EndAt: PChar;
begin
  if not ( nsBackward in FOptions ) then begin
    BeginAt := FBuffer - FMatchLen - FContextLeftLenght;
    EndAt := FBuffer+FContextRightLenght;
  end
  else begin
    BeginAt := FBuffer+1-FContextLeftLenght;
    EndAt := FBuffer+1+FMatchLen+FContextRightLenght;
  end;
  if BeginAt > EndAt then
    raise EWildcardSeekerException.CreateFmt('GetContext::Range Error "BeginAt(%d) > EndAt(%d)"', [Integer(BeginAt), Integer(EndAt)]);
  if BeginAt < FBufferBegin then
    BeginAt := FBufferBegin;
  if EndAt > FBufferEnd then
    EndAt := FBufferEnd;

  SetString( Result, BeginAt, EndAt-BeginAt );
end;

function TWildcardSeeker.GetSearchStr: string;
begin
  Result := StrPas( FSearchBegin );
end;

function TWildcardSeeker.GetRemainingText: string;
begin
  Result := '';
  if not ( nsBackward in FOptions ) then
    Result := StrPas( FBuffer )
  else if FBuffer-FBufferBegin > -1 then
    SetString( Result, FBufferBegin, FBuffer-FBufferBegin+1 );
end;

function TWildcardSeeker.GetCurByte: Cardinal;
begin
  if nsBackward in FOptions then
    Result := FBufferEnd-1 - FBuffer - FMatchLen
  else
    Result := FBuffer - FMatchLen - FBufferBegin;
end;

function TWildcardSeeker.GetEOL: string;
begin
  Result := StrPas( FEOL );
end;

procedure TWildcardSeeker.SetOptions(const Value: TWildcardSeekerOptions);
begin
  FOptions := Value;
  if nsBackward in Value then
    if nsHandleWildCard in Value then
      Search := SearchBackwardWithWildCard
    else
      Search := SearchBackward
  else if nsHandleWildCard in Value then
    Search := SearchForwardWithWildCard
  else
    Search := SearchForward;
end;

procedure TWildcardSeeker.SetText( const Value: string );
begin
  LoadFromBuffer( PChar( Value ) );
end;

procedure TWildcardSeeker.SetSearchStr(const Value: string);
begin
  FreeSearchStr;
  FSearchLen := Length( Value );
  GetMem( FSearchBegin, FSearchLen+1 );
  StrCopy( FSearchBegin, PChar( Value ) );
  FSearch := FSearchBegin;
  FSearchEnd := StrEnd( FSearchBegin );
end;

procedure TWildcardSeeker.SetEOL(const Value: string);
begin
  FreeEOL;
  FEOLLen := Length( Value );
  GetMem( FEOL, FEOLLen+1 );
  StrCopy( FEOL, PChar( Value ) );
end;


procedure TWildcardSeeker.FreeBuffer;
begin
  if Assigned( FBufferBegin ) then begin
    FBufferEnd := nil;
    FBuffer := nil;
    FreeMem( FBufferBegin );
  end;
  if Assigned( FBufferBackup ) then begin
    FreeMem( FBufferBackup );
    FBufferBackup := nil;
  end;
end;

procedure TWildcardSeeker.FreeEOL;
begin
  if Assigned( FEOL ) then begin
    FreeMem( FEOL );
    FEOL := nil;
  end;
end;

procedure TWildcardSeeker.FreeSearchStr;
begin
  if Assigned( FSearchBegin ) then begin
    FreeMem( FSearchBegin );
    FSearchBegin := nil;
    FSearch := nil;
    FSearchEnd := nil;
  end;
end;



{ TWildcardSeeker : public }

constructor TWildcardSeeker.Create;
begin
  EOL := #13#10;
  FContextLeftLenght := 10;
  FContextRightLenght := 20;
  Search := SearchForward;
end;

destructor TWildcardSeeker.Destroy;
begin
  FreeBuffer;
  FreeSearchStr;
  FreeEOL;
  inherited Destroy;
end;

procedure TWildcardSeeker.LoadFromBuffer( const ABuffer: PChar );
begin
  FreeBuffer;
  GetMem( FBufferBegin, StrLen( ABuffer )+1 );
  FBuffer := StrCopy( FBufferBegin, ABuffer );
  FBufferEnd :=  StrEnd( FBufferBegin );
  if FKeepText then begin
    GetMem( FBufferBackup, StrLen( FBufferBegin )+1 );
    StrCopy( FBufferBackup, FBufferBegin );
  end;  
end;

procedure TWildcardSeeker.LoadFromFile(const AFilename: string);
var
  FS: TFileStream;
begin
  if not FileExists( AFilename ) then
    raise EWildcardSeekerException.CreateFmt( 'LoadFromFile::File "%s" not found', [AFilename] );
  FS := TFileStream.Create( AFilename, fmOpenRead );
  try
    LoadFromStream( FS );
  finally
    FS.Free;
  end;
end;

procedure TWildcardSeeker.LoadFromStream(const AStream: TStream);
var
  Size: Int64;
begin
  FreeBuffer;
  Size := AStream.Size;
  GetMem( FBuffer, Size+1 );
  Size := AStream.Read( FBuffer^, Size );
  ( FBuffer+Size )^ := #0;
  FBufferEnd := (FBuffer+Size);
  FBufferBegin := FBuffer;
  if FKeepText then begin
    GetMem( FBufferBackup, StrLen( FBufferBegin )+1 );
    StrCopy( FBufferBackup, FBufferBegin );
  end;  
end;

procedure TWildcardSeeker.StartSearch;
begin
  FMatches := 0;
  FCurLine := 0;
  FCurCol := 0;

  if FSearchLen = 0 then
    raise EWildcardSeekerException.Create( 'StartSearch::Property SearchStr is empty.' );

  if ( FBufferBackup <> FBufferBegin ) and Assigned( FBufferBackup ) then begin
    FreeMem( FBufferBegin );
    GetMem( FBufferBegin, StrLen( FBufferBackup )+1 );
    FBuffer := StrCopy( FBufferBegin, FBufferBackup );
    FBufferEnd := FBuffer + StrLen( FBufferBackup );
  end;

  if nsBackward in FOptions then
    FBuffer := FBufferEnd-1
  else
    FBuffer := FBufferBegin;
end;

procedure TWildcardSeeker.Replace(const S: String);
var
  TempBuff: PChar;
  BufferOffset: Integer;
begin
  TempBuff := nil;
  if not ( nsBackward in FOptions ) then begin
    BufferOffset := FBuffer-FMatchLen - FBufferBegin + Length( S );
    GetMem( TempBuff, ( FBuffer-FMatchLen - FBufferBegin) + Length(S) + ( FBufferEnd - FBuffer ) + 1 );
    FBufferEnd := StrLCopy( StrLCopy( StrLCopy( TempBuff, FBufferBegin, FBuffer-FMatchLen - FBufferBegin )+(FBuffer-FMatchLen - FBufferBegin), PChar( S ), Length( S ) )+Length( S ), FBuffer, FBufferEnd - FBuffer )+(FBufferEnd - FBuffer);
    FreeMem( FBufferBegin );
    FBufferBegin := TempBuff;
    FBuffer := FBufferBegin + BufferOffset;
  end
  else begin
    BufferOffset := FBufferEnd - (FBuffer + FMatchLen) + Length( S );
    if FBuffer < FBufferBegin then
      GetMem( TempBuff, FBufferEnd - (FBuffer+FMatchLen) + Length( S ) + ( FBuffer - FBufferBegin ) + 1 )
    else
      GetMem( TempBuff, FBufferEnd - (FBuffer+FMatchLen) + Length( S ) + ( FBuffer - FBufferBegin ) + 1 );
    FBufferEnd := StrLCopy( StrLCopy( StrLCopy( TempBuff, FBufferBegin, FBuffer-FBufferBegin+1 )+(FBuffer-FBufferBegin+1), PChar( S ), Length( S ) )+Length( S ), FBuffer+FMatchLen+1, FBufferEnd-1 - (FBuffer+FMatchLen) )+ ( FBufferEnd-1 - (FBuffer+FMatchLen) );
    FreeMem( FBufferBegin );
    FBufferBegin := TempBuff;
    FBuffer := FBufferEnd - BufferOffset;
  end;
end;


{ EWildcardSeekerException }
{ EWildcardSeekerException : public }

constructor EWildcardSeekerException.Create(const Msg: string);
begin
  inherited Create( 'TWildcardSeeker.'+Msg );
end;

constructor EWildcardSeekerException.CreateFmt(const Msg: string; const Args: array of const);
begin
  inherited CreateFmt( 'TWildcardSeeker.'+Msg, Args );
end;

procedure TWildcardSeeker.EnableOptions(const CaseSensitive, WholeWords, HandleEOL, HandleWildCard, Backward: Boolean);
  var Opcoes: TWildcardSeekerOptions;
begin
  if CaseSensitive then Include( Opcoes, nsCaseSensitive ) else Exclude( Opcoes, nsCaseSensitive );
  if HandleEOL then Include( Opcoes, nsHandleEOL ) else Exclude( Opcoes, nsHandleEOL );
  if Backward then Include( Opcoes, nsBackward ) else Exclude( Opcoes, nsBackward );
  if HandleWildCard then Include( Opcoes, nsHandleWildCard ) else Exclude( Opcoes, nsHandleWildCard );
  if WholeWords then Include( Opcoes, nsWholeWords ) else Exclude( Opcoes, nsWholeWords );
  SetOptions( Opcoes );
end;

function TWildcardSeeker.SearchForward: Boolean;
begin
  Result := True;
  LineJump := 0;
  FMatchLine := 0;
  FMatchCol := 0;
  FMatchLen := 0;
  FSearch := FSearchBegin;

  if FBufferBegin + FStartAt > FBufferEnd then
    FStartAt := FBufferEnd - FBufferBegin;
  while FStartAt > FBuffer-FBufferBegin do begin
    if ( nsHandleEOL in FOptions ) and ( FBuffer^ = FEOL^ ) and ( StrLComp( FBuffer, FEOL, FEOLLen ) = 0 ) then begin
      Inc( FBuffer, FEOLLen );
      Inc( FCurLine );
      FCurCol := 0;
      Continue;
    end
    else
      Inc( FCurCol );
    Inc( FBuffer );
  end;

  while FBuffer <> FBufferEnd do begin
    if ( (nsCaseSensitive in FOptions ) and ( FBuffer^ = FSearch^ ) ) or ( not(nsCaseSensitive in FOptions ) and ( AnsiUpCase( FBuffer^ ) = AnsiUpCase( FSearch^ ) ) ) then begin
      Inc( FMatchLen );
      Inc( FSearch );
      if Result then begin
        Result := False;
        FMatchCol := FCurCol;
        FMatchLine := FCurLine;
        if ( nsWholeWords in FOptions ) and ( FBuffer > FBufferBegin ) and not ( (FBuffer-1)^ in WhiteSpaces ) then begin
          FSearch := FSearchBegin;
          FMatchLen := 0;
          FMatchLine := 0;
          FMatchCol := 0;
        end;
      end;
      if ( nsWholeWords in FOptions ) and ( FMatchLen = FSearchLen ) and ( FBuffer < FBufferEnd-1 ) and not ( (FBuffer+1)^ in WhiteSpaces ) then begin
        FSearch := FSearchBegin;
        FMatchLen := 0;
        FMatchLine := 0;
        FMatchCol := 0;
      end;

      if ( nsHandleEOL in FOptions ) and ( FBuffer^ = FEOL^ ) and ( StrLComp( FBuffer, FEOL, FEOLLen ) = 0 ) then begin
        Inc( FCurLine );
        FCurCol := 0;
        LineJump := FEOLLen-1;
      end
      else if LineJump = 0 then
        Inc( FCurCol )
      else
        Dec( LineJump );

      Inc( FBuffer );
      if FSearch^ = #0 then begin
        Result := True;
        Inc( FMatches );
        Exit;
      end;
    end
    else begin
      Result := True;
      FMatchLen := 0;
      FMatchLine := 0;
      FMatchCol := 0;
      
      if FSearch <> FSearchBegin then begin
        FSearch := FSearchBegin;
        Continue;
      end;

      if ( nsHandleEOL in FOptions ) and ( FBuffer^ = FEOL^ ) and ( StrLComp( FBuffer, FEOL, FEOLLen ) = 0 ) then begin
        Inc( FCurLine );
        FCurCol := 0;
        LineJump := FEOLLen-1;
      end
      else if LineJump = 0 then
        Inc( FCurCol )
      else
        Dec( LineJump );

      Inc( FBuffer )        
    end;
  end;
  Result := False;
end;

function TWildcardSeeker.SearchForwardWithWildCard: Boolean;
begin
  Result := True;
  Jump := 1;
  LineJump := 0;
  FMatchLine := 0;
  FMatchCol := 0;
  FMatchLen := 0;
  FSearch := FSearchBegin;

  if FBufferBegin + FStartAt > FBufferEnd then
    FStartAt := FBufferEnd - FBufferBegin;
  while FStartAt > FBuffer-FBufferBegin do begin
    if ( nsHandleEOL in FOptions ) and ( FBuffer^ = FEOL^ ) and ( StrLComp( FBuffer, FEOL, FEOLLen ) = 0 ) then begin
      Inc( FBuffer, FEOLLen );
      Inc( FCurLine );
      FCurCol := 0;
      Continue;
    end
    else
      Inc( FCurCol );
    Inc( FBuffer );
  end;

  while FBuffer <> FBufferEnd do begin
    if FSearch^ = '?' then begin
      Inc( FMatchLen );
      Inc( FSearch );
    end
    else if FSearch^ = '*' then begin
      if (FSearch+Jump)^ = '?' then begin
        Inc( FMatchLen );
        Inc( Jump );
      end
      else if (FSearch+Jump)^ = '*' then begin
        Inc( FSearch, Jump );
        Jump := 1;
        continue;
      end
      else if ( ( nsCaseSensitive in FOptions ) and ( FBuffer^ = (FSearch+Jump)^ ) ) or ( not( nsCaseSensitive in FOptions ) and ( AnsiUpCase( FBuffer^ ) = AnsiUpCase( (FSearch+Jump)^ ) )  ) then begin
        Inc( FMatchLen );
        Inc( Jump );
      end
      else
        Inc( FMatchLen );
      if (FSearch+Jump)^ = #0 then begin
        if (FSearch+Jump-1)^ = '*' then begin
          Inc( FMatchLen, FBufferEnd-FBuffer - 1 );
          FBuffer := FBufferEnd - 1;
        end;
        FSearch := FSearchEnd;
      end;
    end
    else if ( ( nsCaseSensitive in FOptions ) and ( FBuffer^ = FSearch^ ) ) or ( not ( nsCaseSensitive in FOptions ) and ( AnsiUpCase( FBuffer^ ) = AnsiUpCase( FSearch^ ) ) ) then begin
      Inc( FMatchLen );
      Inc( FSearch );
    end
    else begin
      Result := True;
      FMatchLen := 0;
      FMatchLine := 0;
      FMatchCol := 0;
      Jump := 1;
      if FSearch <> FSearchBegin then begin
        FSearch := FSearchBegin;
        Continue;
      end;

      if ( nsHandleEOL in FOptions ) and ( FBuffer^ = FEOL^ ) and ( StrLComp( FBuffer, FEOL, FEOLLen ) = 0 ) then begin
        Inc( FCurLine );
        FCurCol := 0;
        LineJump := FEOLLen-1;
      end
      else if LineJump = 0 then
        Inc( FCurCol )
      else
        Dec( LineJump );

      Inc( FBuffer );
      continue;
    end;

    if Result then begin
      Result := False;
      FMatchCol := FCurCol;
      FMatchLine := FCurLine;
    end;

    if ( nsHandleEOL in FOptions ) and ( FBuffer^ = FEOL^ ) and ( StrLComp( FBuffer, FEOL, FEOLLen ) = 0 ) then begin
      Inc( FCurLine );
      FCurCol := 0;
      LineJump := FEOLLen-1;
    end
    else if LineJump = 0 then
      Inc( FCurCol )
    else
      Dec( LineJump );

    Inc( FBuffer );

    if FSearch^ = #0 then begin
      Result := True;
      Inc( FMatches );
      Exit;
    end;
  end;
  Result := False;
end;

function TWildcardSeeker.SearchBackward: Boolean;
begin
  Result := True;
  LineJump := 0;
  FMatchLine := 0;
  FMatchCol := 0;
  FMatchLen := 0;
  FSearch := FSearchEnd-1;

  if FBufferEnd - FStartAt < FBufferBegin then
    FStartAt := FBufferEnd - FBufferBegin;
  while FStartAt > FBufferEnd-1 - FBuffer do begin
    if ( nsHandleEOL in FOptions ) and ( FBuffer-FBufferBegin >= FEOLLen ) and StrLRComp( FBuffer, FEOL+FEOLLen-1, FEOL ) then begin
      Dec( FBuffer, FEOLLen );
      Inc( FCurLine );
      FCurCol := 0;
      Continue;
    end
    else
      Inc( FCurCol );
    Dec( FBuffer );
  end;

  while FBuffer <> FBufferBegin-1 do begin
    if ( (nsCaseSensitive in FOptions ) and ( FBuffer^ = FSearch^ ) ) or ( not(nsCaseSensitive in FOptions ) and ( AnsiUpCase( FBuffer^ ) = AnsiUpCase( FSearch^ ) ) ) then begin
      Inc( FMatchLen );
      Dec( FSearch );
      if Result then begin
        Result := False;
        FMatchCol := FCurCol;
        FMatchLine := FCurLine;

        if ( nsWholeWords in FOptions ) and ( FBuffer < FBufferEnd-1 ) and not ( (FBuffer+1)^ in WhiteSpaces ) then begin
          FSearch := FSearchEnd-1;
          FMatchLen := 0;
          FMatchLine := 0;
          FMatchCol := 0;
        end;
      end;
      if ( nsWholeWords in FOptions ) and ( FMatchLen = FSearchLen ) and ( FBuffer > FBufferBegin ) and not ( (FBuffer-1)^ in WhiteSpaces ) then begin
        FSearch := FSearchEnd-1;
        FMatchLen := 0;
        FMatchLine := 0;
        FMatchCol := 0;
      end;

      if ( nsHandleEOL in FOptions ) and ( FBuffer-FBufferBegin >= FEOLLen ) and StrLRComp( FBuffer, FEOL+FEOLLen-1, FEOL ) then begin
        Inc( FCurLine );
        FCurCol := 0;
        LineJump := FEOLLen-1;
      end
      else if LineJump = 0 then
        Inc( FCurCol )
      else
        Dec( LineJump );

      Dec( FBuffer );
      if FSearch = FSearchBegin-1 then begin
        Result := True;
        Inc( FMatches );
        Exit;
      end;
    end
    else begin
      Result := True;
      FMatchLen := 0;
      FMatchLine := 0;
      FMatchCol := 0;

      if FSearch <> FSearchEnd-1 then begin
        FSearch := FSearchEnd-1;
        Continue;
      end;

      if ( nsHandleEOL in FOptions ) and ( FBuffer-FBufferBegin >= FEOLLen ) and StrLRComp( FBuffer, FEOL+FEOLLen-1, FEOL ) then begin
        Inc( FCurLine );
        FCurCol := 0;
        LineJump := FEOLLen-1;
      end
      else if LineJump = 0 then
        Inc( FCurCol )
      else
        Dec( LineJump );

      Dec( FBuffer );
    end;
  end;
  Result := False;
end;

function TWildcardSeeker.SearchBackwardWithWildCard: Boolean;
begin
  Result := True;
  Jump := 1;
  LineJump := 0;
  FMatchLine := 0;
  FMatchCol := 0;
  FMatchLen := 0;
  FSearch := FSearchEnd-1;

  if FBufferEnd - FStartAt < FBufferBegin then
    FStartAt := FBufferEnd - FBufferBegin;
  while FStartAt > FBufferEnd-1 - FBuffer do begin
    if ( nsHandleEOL in FOptions ) and ( FBuffer^ = FEOL^ ) and ( StrLComp( FBuffer, FEOL, FEOLLen ) = 0 ) then begin
      Dec( FBuffer, FEOLLen );
      Inc( FCurLine );
      FCurCol := 0;
      Continue;
    end
    else
      Inc( FCurCol );
    Dec( FBuffer );
  end;

  while FBuffer <> FBufferBegin-1 do begin
    if FSearch^ = '?' then begin
      Inc( FMatchLen );
      Dec( FSearch );
    end
    else if FSearch^ = '*' then begin
      if (FSearch-Jump)^ = '?' then begin
        Inc( FMatchLen );
        Inc( Jump );
      end
      else if (FSearch-Jump)^ = '*' then begin
        Dec( FSearch, Jump );
        Jump := 1;
        continue;
      end
      else if ( ( nsCaseSensitive in FOptions ) and ( FBuffer^ = (FSearch-Jump)^ ) ) or ( not( nsCaseSensitive in FOptions ) and ( AnsiUpCase( FBuffer^ ) = AnsiUpCase( (FSearch-Jump)^ ) )  ) then begin
        Inc( FMatchLen );
        Inc( Jump );
      end
      else
        Inc( FMatchLen );
      if (FSearch-Jump) = FSearchBegin-1 then begin
        if (FSearch-Jump+1)^ = '*' then begin
          Inc( FMatchLen, FBuffer-FBufferBegin );
          FBuffer := FBufferBegin;
        end;
        FSearch := FSearchBegin-1;
      end;
    end
    else if ( ( nsCaseSensitive in FOptions ) and ( FBuffer^ = FSearch^ ) ) or ( not ( nsCaseSensitive in FOptions ) and ( AnsiUpCase( FBuffer^ ) = AnsiUpCase( FSearch^ ) ) ) then begin
      Inc( FMatchLen );
      Dec( FSearch );
    end
    else begin
      Result := True;
      FMatchLen := 0;
      FMatchLine := 0;
      FMatchCol := 0;
      Jump := 1;

      if FSearch <> FSearchEnd-1 then begin
        FSearch := FSearchEnd-1;
        Continue;
      end;

      if ( nsHandleEOL in FOptions ) and ( FBuffer-FBufferBegin >= FEOLLen ) and StrLRComp( FBuffer, FEOL+FEOLLen-1, FEOL ) then begin
        Inc( FCurLine );
        FCurCol := 0;
        LineJump := FEOLLen-1;
      end
      else if LineJump = 0 then
        Inc( FCurCol )
      else
        Dec( LineJump );

      Dec( FBuffer );
      Continue;
    end;

    if Result then begin
      Result := False;
      FMatchCol := FCurCol;
      FMatchLine := FCurLine;
    end;

    if ( nsHandleEOL in FOptions ) and ( FBuffer-FBufferBegin >= FEOLLen ) and StrLRComp( FBuffer, FEOL+FEOLLen-1, FEOL ) then begin
      Inc( FCurLine );
      FCurCol := 0;
      LineJump := FEOLLen-1;
    end
    else if LineJump = 0 then
      Inc( FCurCol )
    else
      Dec( LineJump );

    Dec( FBuffer );
    if FSearch = FSearchBegin-1 then begin
      Result := True;
      Inc( FMatches );
      Exit;
    end;
  end;
  Result := False;
end;

end.
