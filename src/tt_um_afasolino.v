module tt_um_afasolino (
    input  wire [7:0] ui_in,    // Dedicated inputs 
    output wire [7:0] uo_out,   // Dedicated outputs 
    input  wire [7:0] uio_in,   // IOs: Bidirectional Input path
    output wire [7:0] uio_out,  // IOs: Bidirectional Output path
    output wire [7:0] uio_oe,   // IOs: Bidirectional Enable path (active high: 0=input, 1=output)
    input  wire       ena,      // will go high when the design is enabled
    input  wire       clk,      // clock
    input  wire       rst_n     // reset_n - low to reset
);


assign uio_oe = {!ena,!ena,!ena,!ena,!ena,!ena,!ena,!ena};

topmodule topmodule(
  .InPE(ui_in[6:3]),
  .clk(clk),
  .enable(ena),
  .NEP(uio_in[3:0]),
  .SignExEn(ui_in[7]),
  .rst(rst),
  .InputSel(ui_in[1:0]),
  .w(ui_in[2:0]),
  .EPcount(uio_in[7:4]),
  .o(uio_out)
);

endmodule

module topmodule (
  input [3:0] InPE,
  input clk,
  input enable,
  input clear,
  input NEP,
  input SignExEn,
  input rst,
  input [1:0] InputSel,
  input [2:0] w,
  input [3:0] EPcount,
  output reg [7:0] o
);


  wire [2:0] expResult;
  wire [17:0] eResult;
  wire [9:0] result;
  
   always @* begin
     case (InputSel)
        4'b00: o <= result[7:0];
        4'b01: o <= {eResult[4:0],expResult};
        4'b10: o <= eResult[12:5];
        default: o <= {result[9:8], eResult[17:13]};
     endcase
    end
  wire s;
  wire [1:0] c;
  wire [2:0] w_reg0;
  reg [2:0] w_reg;
  wire [3:0] InPE_reg0;
  reg [3:0] InPE_reg;
  reg [4:0] InPE_aux;
  reg [5:0] saux,a, a_aux;
  
  D_FF #(3) w_FF (
    .d(w),
    .clk(clk),
    .enable(enable),
    .clear(clear),
    .rst(rst),
    .q(w_reg0)
  );
  
always @* begin
     w_reg=w_reg0;
end

  D_FF #(4) InPE_FF (
    .d(InPE),
    .clk(clk),
    .enable(enable),
    .rst(rst),
    .clear(clear),
    .q(InPE_reg0)
  );
always @* begin
     InPE_reg=InPE_reg0;
end
  always @(*) begin
    case (SignExEn)
      1'b1: InPE_aux = {InPE_reg[3], InPE_reg};
      default: InPE_aux = {1'b0, InPE_reg};
    endcase
  end

  BoothEncoderMod Encoder (
    .x2(w_reg[2]),
    .x1(w_reg[1]),
    .x0(w_reg[0]),
    .sign(s),
    .C(c)
  );

  always @* begin
    case (c)
      2'b10: a_aux = {InPE_aux, 1'b0};
      2'b01: a_aux = {InPE_aux[4], InPE_aux};
      default: a_aux = 6'b0;
    endcase
  end

  always @* begin
    saux<= {s,s,s,s,s,s};
      a <= saux ^ a_aux;
  end

  DAAforNE DAA (
    .x(a),
    .clk(clk),
    .enable(enable),
    .clear(clear),
    .NEP(NEP),
    .Cin(s),
    .rst(rst),
    .EPcount(EPcount),
    .expResult(expResult),
    .eResult(eResult),
    .result(result)
  );

endmodule

module BoothEncoderMod (
  input x2,
  input x1,
  input x0,
  output reg sign,
  output reg [1:0] C
);

  always @(x2 or x1 or x0) begin
    sign = x2 & ~(x2 & x1 & x0);
    C[0] = x0 ^ x1;
    C[1] = (x0 & x1 & ~x2) | (x2 & ~x1 & ~x0);
  end

endmodule

module CU_MAC2(
    input clk,
    input enable,
    input clear,
    input a,
    input b,
    input [1:0] res,
    input [2:0] Nshift,
    input co,
    input rst,
    output reg inc,
    output reg decr
);

reg inc_aux, decr_aux;
wire inc_reg, decr_reg;

always @(posedge clk or posedge clear) begin
    if (clear)
        inc_aux <= 1'b0;
    else begin
        inc_aux <= (~a & ~b & ~co & res[1]) | (a & b & co & ~res[1]) & ~Nshift;
    end
end

always @(posedge clk or posedge clear) begin
    if (clear)
        decr_aux <= 1'b0;
    else begin
        decr_aux <= ~inc_aux & (~co & (res[1] ^ res[0])) & Nshift;
    end
end

D_FF #(1) inc_FF (
    .d(inc_aux),
    .clk(clk),
    .enable(enable),
    .rst(rst),
    .clear(clear),
    .q(inc_reg)
);

D_FF #(1) decr_FF (
    .d(decr_aux),
    .clk(clk),
    .enable(enable),
        .rst(rst),

    .clear(clear),
    .q(decr_reg)
);

always @* begin
    inc <= inc_reg;
    decr <= decr_reg;
end

endmodule

module D_FF #(parameter N=4) (
    input [N-1:0] d,
    input clk,
    input enable,
    input clear,
    input rst,
    output reg [N-1:0] q
);

//always @(posedge clk or negedge rst) begin
//    if (!rst)
//        q <= {N{1'b0}};
//    else if (enable)
//        if (clear)
//           q <= {N{1'b0}};
//        else
//            q <= d;
//end

always @(posedge clk or negedge rst) begin
    if (!rst)
        q <= {N{1'b0}};
    else if (enable)
        if (clear)
           q <= {N{1'b0}};
        else
            q <= d;
end
//always @(posedge clk or negedge rst) begin
//    if (!rst)
//        q <= {N{1'b0}};
//    else if (clear)
//        q <= {N{1'b0}};
//        else if (enable)
//            q <= d;
//end



endmodule

module DAAforNE (
    input [5:0] x,
    input clk,
    input enable,
    input clear,
    input NEP,
    input Cin,
    input rst,
    input [3:0] EPcount,
    output reg [2:0] expResult,
    output reg [17:0] eResult,
    output reg [9:0] result
);

wire [17:0] inSaveFF;
reg [5:0] x_reg;
wire [9:0] b, res_reg, aux;
reg [9:0] a, res, inc_aux, decr_aux;
reg [10:0] res_aux, CinAux;
wire [9:0] r_a, OutAllined1;
reg [9:0] r_a1, r_a2;
wire [7:0] x_reg_se_aux;
reg [7:0] x_reg_se;
reg IncOrDec_sel, IncOrDec_sel2, selAllineMUX2, selSaveMUX, co;
wire NEP_reg2, inc, decr, co_reg;
reg Cin_reg;
reg En0, En1, En2, En3, En4, En5, En6, En7, En8;
wire [2:0] N_shift;
wire [1:0] r_a_save_aux;
reg [1:0] r_a_save;

//D_FF #(6) x_FF (.d(x), .clk(clk), .enable(enable), .clear(clear), .q(x_reg));
//D_FF #(1) Cin_FF (.d(Cin), .clk(clk), .enable(enable), .clear(clear), .q(Cin_reg));
//D_FF #(1) NEP_FF (.d(NEP), .clk(clk), .enable(enable), .clear(clear), .q(NEP_reg));
//D_FF #(1) NEP_FF2 (.d(NEP_reg), .clk(clk), .enable(enable), .clear(clear), .q(NEP_reg2));
D_FF #(1) NEP_FF2 (.d(NEP), .clk(clk),     .rst(rst), .enable(enable), .clear(clear), .q(NEP_reg2));

always @* begin
    x_reg = x;
    Cin_reg = Cin;
end

always @* begin
    case (x_reg[5])
        1'b1: x_reg_se = {2'b11, x_reg};
        default: x_reg_se = {2'b00, x_reg};
    endcase
end

ShiftRight_2  LogShift (.i(x_reg_se), .N_shift(N_shift), .o(x_reg_se_aux));
always @* begin
    case (x_reg_se_aux[7])
        1'b1: a = {2'b11, x_reg_se_aux};
        default: a = {2'b00, x_reg_se_aux[6:0]};
    endcase
end
ShiftCounter2 ShiftCounter (.clk(clk), .clear(clear), .enable(enable), .inc(inc), .dec(decr), .shiftCount(N_shift));

always @* begin
    CinAux = {10'b0000000000, Cin_reg};
end

always @* begin
    res_aux = $signed(a) + $signed(b) + $signed(CinAux);
    res = res_aux[9:0];
    co = res_aux[10];
end

CU_MAC2 CU (.clk(clk), .clear(clear), .enable(enable), .a(a[9]), .b(b[9]), .res(res[9:8]), .Nshift(N_shift), .co(co), .inc(inc), .decr(decr));

D_FF #(10) res_FF (.d(res), .clk(clk), .enable(enable),     .rst(rst), .clear(clear), .q(res_reg));
D_FF #(1) Co_FF (.d(co), .clk(clk), .enable(enable),     .rst(rst), .clear(clear), .q(co_reg));

always @* begin
    IncOrDec_sel = ~inc & decr;
    inc_aux = {co_reg, res_reg[9:1]};
    decr_aux = {res_reg[8:0], 1'b0};
end

Mux_2 #(10) IncOrDec_MUX (.i0(inc_aux), .i1(decr_aux), .sel(IncOrDec_sel), .o(aux));
always @* begin
    IncOrDec_sel2 = inc | decr;
end
Mux_2 #(10) IncOrDec_MUX2 (.i0(res_reg), .i1(aux), .sel(IncOrDec_sel2), .o(r_a));
always @* begin
    r_a1 = {r_a[9], r_a[9:1]};
    r_a2 = {r_a[9], r_a[9], r_a[9:2]};
end
Mux_2 #(10) AllignMUX1 (.i0(r_a2), .i1(r_a1), .sel(N_shift[0]), .o(OutAllined1));

always @* begin 
    selAllineMUX2 = ~|N_shift[2:1] & NEP_reg2;
end
Mux_2 #(10) AllignMUX2 (.i0(r_a), .i1(OutAllined1), .sel(selAllineMUX2), .o(b));

always @* begin 
    r_a_save = {r_a[0], 1'b0};
    selSaveMUX = N_shift[0];
end

Mux_2 #(2) SaveMUX (.i0(r_a[1:0]), .i1(r_a_save), .sel(selSaveMUX), .o(r_a_save_aux));

always @* begin
     En0 = (EPcount == 4'b0000 && ~|N_shift[2:1] && NEP == 1'b1);
     En1 = (EPcount == 4'b0001 && ~|N_shift[2:1]  && NEP == 1'b1);
     En2 = (EPcount == 4'b0010 && ~|N_shift[2:1]  && NEP == 1'b1);
     En3 = (EPcount == 4'b0011 && ~|N_shift[2:1]  && NEP == 1'b1);
     En4 = (EPcount == 4'b0100 && ~|N_shift[2:1]  && NEP == 1'b1);
     En5 = (EPcount == 4'b0101 && ~|N_shift[2:1]  && NEP == 1'b1);
     En6 = (EPcount == 4'b0110 && ~|N_shift[2:1]  && NEP == 1'b1);
     En7 = (EPcount == 4'b0111 && ~|N_shift[2:1]  && NEP == 1'b1);
     En8 = (EPcount == 4'b1000 && ~|N_shift[2:1]  && NEP == 1'b1);
end
// Aggiungi tutti gli altri Save_FF con i rispettivi ENx
D_FF #(2) Save_FF0 (.d(r_a_save_aux), .clk(clk), .enable(En0), .clear(clear),     .rst(rst), .q(inSaveFF[1:0]));
D_FF #(2) Save_FF1 (.d(r_a_save_aux), .clk(clk), .enable(En1), .clear(clear), .rst(rst),.q(inSaveFF[3:2]));
D_FF #(2) Save_FF2 (.d(r_a_save_aux), .clk(clk), .enable(En2), .clear(clear), .rst(rst),.q(inSaveFF[5:4]));
D_FF #(2) Save_FF3 (.d(r_a_save_aux), .clk(clk), .enable(En3), .clear(clear), .rst(rst),.q(inSaveFF[7:6]));
D_FF #(2) Save_FF4 (.d(r_a_save_aux), .clk(clk), .enable(En4), .clear(clear), .rst(rst),.q(inSaveFF[9:8]));
D_FF #(2) Save_FF5 (.d(r_a_save_aux), .clk(clk), .enable(En5), .clear(clear), .rst(rst),.q(inSaveFF[11:10]));
D_FF #(2) Save_FF6 (.d(r_a_save_aux), .clk(clk), .enable(En6), .clear(clear), .rst(rst),.q(inSaveFF[13:12]));
D_FF #(2) Save_FF7 (.d(r_a_save_aux), .clk(clk), .enable(En7), .clear(clear), .rst(rst),.q(inSaveFF[15:14]));
D_FF #(2) Save_FF8 (.d(r_a_save_aux), .clk(clk), .enable(En8), .clear(clear), .rst(rst),.q(inSaveFF[17:16]));

always @* begin
    eResult=inSaveFF;
    result=b;
    expResult=N_shift;
end

endmodule

module Mux_2 #(parameter N=4) (
    input [N-1:0] i0,
    input [N-1:0] i1,
    output reg [N-1:0] o,
    input sel
);

always @* begin
    if (sel)
        o = i1;
    else
        o = i0;
end

endmodule

module ShiftCounter2(
    input inc,
    input dec,
    input clk,
    input clear,
    input enable,
    output reg [2:0] shiftCount
);

reg [2:0] incDecr, a, b, res;
reg incDecr_sel, idle_sel;

always @* begin
    incDecr_sel <= ~inc & dec;
    idle_sel <= inc | dec;
    res=a+b;
    shiftCount <= res;
end

always @* begin
    if (incDecr_sel)
        incDecr <= 3'b111;
    else
        incDecr <= 3'b001;
end

always @* begin
    if (idle_sel)
        a <=incDecr;
    else
        a <= 3'b0;
end

always @(posedge clk or posedge clear) begin
    if (clear)
        b <= 3'b0;
    else if (enable)
        b <= res;
end

endmodule


module ShiftRight_2(
    input [7:0] i,
    output reg [7:0] o,
    input [2:0] N_shift
);

wire [7:0] i_4_in, i_4_out;
wire [7:0] i_2_in, i_2_out;
wire [7:0] i_1_out;
reg [7:0] i_1_in;

Log_shifter #(4) shifter_4 
(
    .i(i),
    .o(i_4_in)
);

Mux_2 #(8) shift_4_MUX (
    .i0(i),
    .i1(i_4_in),
    .o(i_4_out),
    .sel(N_shift[2])
);

Log_shifter #(2) shifter_2 (
    .i(i_4_out),
    .o(i_2_in)
);

Mux_2 #(8) shift_2_MUX (
    .i0(i_4_out),
    .i1(i_2_in),
    .o(i_2_out),
    .sel(N_shift[1])
);

always @* begin
     i_1_in = {i[7], i_2_out[7:1]};
end

Mux_2 #(8) shift_1_MUX (
    .i0(i_2_out),
    .i1(i_1_in),
    .o(i_1_out),
    .sel(N_shift[0])
);

always @* begin
    o = i_1_out;
end

endmodule



module Log_shifter #(parameter N=4)(
    input [7:0] i,
    output reg [7:0] o
);

wire [7:0] ones = {1'b1,1'b1,1'b1,1'b1,1'b1,1'b1,1'b1,1'b1};
wire [7:0] zeros = {1'b0,1'b0,1'b0,1'b0,1'b0,1'b0,1'b0,1'b0};

always @* begin
    if (i[7])
        o = {ones[N-1:0],i[7:N]};
    else
        o = {zeros[N-1:0],i[7:N]};
end

endmodule