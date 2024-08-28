//counter RTL

module counter(clk, rst, load, mode, data_in, data_out);
	input rst,clk,load, mode;
	input [3:0] data_in;
	output reg [3:0]data_out;
	
always@(posedge clk)
begin 
	if(rst)
		begin
			data_out<=4'd0;
		end
	else if(load)
		begin
			data_out<=data_in;
		end
	else if(mode)
		begin
			if(data_out==4'd11)
				data_out<=4'd0;
			else
				data_out<=data_out+1'b1;
		end
	else
		begin
			if(data_out==4'd0)
				data_out<=4'd11;
			else
				data_out<=data_out-1'b1;
			end
		end
endmodule

//Counter package

package pkg;
	int no_of_transactions = 1;
endpackage: pkg

// interface

interface counter_if(input bit clock);
   
	logic rst, load, mode;
	logic [3:0] data_in, data_out;

   clocking wr_drv_cb@(posedge clock);
      default input #1 output #1;
        output rst;
	output load;
	output mode;
        output data_in;
   endclocking: wr_drv_cb

   clocking wr_mon_cb@(posedge clock);
      default input #1 output #1;
     	input rst;
	input load;
	input mode;
        input data_in;
   endclocking: wr_mon_cb
   
   clocking rd_mon_cb@(posedge clock);
      default input #1 output #1;
      input data_out;
   endclocking: rd_mon_cb

   modport WR_DRV_MP (clocking wr_drv_cb);
   modport WR_MON_MP (clocking wr_mon_cb);
   modport RD_MON_MP (clocking rd_mon_cb);
    

endinterface: counter_if

//class transaction:
class counter_trans;
   
   rand bit load;
   rand bit mode;
   rand bit [3:0] data_in;
   rand bit rst;

   logic [3:0] data_out;
	
	constraint RST {rst dist{0:=10,1:=1};}
	constraint LOAD {load dist{0:=4, 1:=1};}
	constraint MODE {mode dist{0:=10, 1:=10};}
	constraint DATA {data_in inside{[1:10]};}
 
   static int trans_id;

  virtual function void display(input string message);
      $display("=============================================================");
      $display("The input string message : %s",message);
            $display("\n_______________________________");
            $display($time, "rst -", rst,"load - ", load,"mode - ", mode,"data_in - ","output - ", data_out);
            $display("\n_______________________________");
      $display("=============================================================");
   endfunction: display

   function void post_randomize();
      trans_id++;
   endfunction: post_randomize

endclass: counter_trans

//counter generator class: 

class counter_gen;

counter_trans gen_trans;
counter_trans data2send;

   mailbox #(counter_trans) gen2wr;

   function new(mailbox #(counter_trans) gen2wr);
      this.gen_trans = new;
      this.gen2wr    = gen2wr;
   endfunction: new

   virtual task start();
      fork 
         begin
            for(int i=0; i<no_of_transactions; i++)
               begin      
                  assert(gen_trans.randomize());
                  data2send = new gen_trans;
                  gen2wr.put(data2send);
               end
          end
     join_none
   endtask: start

endclass: counter_gen


//class write driver:

class counter_write_drv;
   virtual counter_if.WR_DRV_MP wr_drv_if;
   counter_trans data2duv;
   mailbox #(counter_trans) gen2wr;  

   function new(virtual counter_if.WR_DRV_MP wr_drv_if,
                mailbox #(counter_trans) gen2wr);
      this.wr_drv_if = wr_drv_if;
      this.gen2wr    = gen2wr;
   endfunction: new

   virtual task drive();

      @(wr_drv_if.wr_drv_cb);
	begin
	wr_drv_if.wr_drv_cb.rst      <= data2duv.rst;
 	wr_drv_if.wr_drv_cb.load      <= data2duv.load;
	wr_drv_if.wr_drv_cb.mode  <= data2duv.mode;
	wr_drv_if.wr_drv_cb.data_in    <= data2duv.data_in;
	end
         
   endtask: drive
  
   virtual task start();
      fork
         forever
            begin
               gen2wr.get(data2duv);
               drive();
            end
      join_none
   endtask: start

endclass: counter_write_drv

//write monitor :

class counter_write_mon;

   virtual counter_if.WR_MON_MP wr_mon_if;

   counter_trans data2rm;

   mailbox #(counter_trans) wr2rm;

   function new(virtual counter_if.WR_MON_MP wr_mon_if,
                mailbox #(counter_trans) wr2rm);
      this.wr_mon_if = wr_mon_if;
      this.wr2rm    = wr2rm;
      this.data2rm  =new();
   endfunction: new


   virtual task monitor();
      @(wr_mon_if.wr_mon_cb)
      begin
         data2rm.rst= wr_mon_if.wr_mon_cb.rst;
         data2rm.load =  wr_mon_if.wr_mon_cb.load;
         data2rm.mode= wr_mon_if.wr_mon_cb.mode;
	 data2rm.data_in= wr_mon_if.wr_mon_cb.data_in;
         data2rm.display("DATA FROM WRITE MONITOR");
      
      end
   endtask: monitor
        
   virtual task start();
      fork
         forever
            begin
               monitor(); 
               wr2rm.put(data2rm);
            end
      join_none
   endtask: start

endclass:counter_write_mon


//counter read monitor:

class counter_read_mon;
   virtual counter_if.RD_MON_MP rd_mon_if;
   counter_trans data2rm;
   counter_trans data2sb;

   //mailbox #(counter_trans) mon2rm;
   mailbox #(counter_trans) mon2sb;
   
   function new(virtual counter_if.RD_MON_MP rd_mon_if,
                mailbox #(counter_trans) mon2sb);
      this.rd_mon_if = rd_mon_if;
      this.mon2sb    = mon2sb;
      this.data2rm    = new;
   endfunction: new


   virtual task monitor();
      @(rd_mon_if.rd_mon_cb);
      begin
         data2rm.data_out = rd_mon_if.rd_mon_cb.data_out;
         data2rm.display("DATA FROM READ MONITOR");    
      end
   endtask: monitor
   
      
   virtual task start();
$display("\nREAD START");
      fork
         forever
            begin
               monitor();
               data2sb = new data2rm;
               mon2sb.put(data2sb);
            end
      join_none
   endtask: start

endclass: counter_read_mon


// Reference model :

class counter_model;
   
   counter_trans mon_data = new();
   
   mailbox #(counter_trans) wr2rm;
   mailbox #(counter_trans) rm2sb;

   function new(mailbox #(counter_trans) wr2rm,
                mailbox #(counter_trans) rm2sb);
      this.wr2rm = wr2rm;
      this.rm2sb = rm2sb;
   endfunction: new
   
   task counter(counter_trans mon_data);
	begin
		if(mon_data.rst)
		mon_data.data_out<=4'd0;
		
		else if(mon_data.load)
		mon_data.data_out <= mon_data.data_in;

		else if(mon_data.mode)
		begin
			if(mon_data.data_out ==4'd11)
			mon_data.data_out <= 4'd0;
			else	
			mon_data.data_out <= mon_data.data_out +1;
		end
		else 
		begin
			if(mon_data.data_out ==4'd0)
			mon_data.data_out <= 4'd11;
			else	
			mon_data.data_out <= mon_data.data_out - 1;
		end
	end
	endtask: counter

     task start();
      $display("RM start");
      fork
         begin
                  forever 
                     begin
			counter(mon_data);
                        wr2rm.get(mon_data);
                        rm2sb.put(mon_data);
                     end
           end
      join_none
   endtask: start


endclass: counter_model


//scoreboard:

class counter_sb;
   
   event DONE; 

   int data_verified = 0;

   counter_trans rmdata;  
   counter_trans sbdata;
   counter_trans cov_data;
 
   mailbox #(counter_trans) rm2sb;     
   mailbox #(counter_trans) rd2sb;   
         
   function new(mailbox #(counter_trans) rm2sb,
                mailbox #(counter_trans) rd2sb);
      this.rm2sb    = rm2sb;
      this.rd2sb = rd2sb;     
   endfunction: new

//coverage code

covergroup coverage;
RST : coverpoint cov_data.rst;
MODE : coverpoint cov_data.mode;
LOAD : coverpoint cov_data.load;
DATA_IN : coverpoint cov_data.data_in{bins a = {[1:10]};}
CR : cross RST,MODE,LOAD,DATA_IN;
endgroup: coverage
   
   virtual task start();
      fork
         while(1)
            begin
               rm2sb.get(rmdata);
               rd2sb.get(sbdata);    
               check(sbdata);
            end
      join_none
   endtask: start

 
   virtual task check(counter_trans rddata);
      if(rmdata.data_out == rddata.data_out) 
         $display ("\n Data Verified");
         else
	 $display ("\n Data Mismatch");
 //shallow copy rmdata to cov data
	cov_data = new rmdata;
	coverage.sample();
	data_verified++;
                          
            if(data_verified == no_of_trasactions)
               begin             
                  ->DONE;
               end
   endtask: check



   virtual function void report();
      $display(" ------------------------ SCOREBOARD REPORT ----------------------- \n ");
      $display("\nData Verified : %0d", data_verified);
      $display(" ------------------------ScoreBoard Report------------------------------------------ \n ");
   endfunction: report
    
endclass: counter_sb


//class counter_env

class counter_env;

   virtual counter_if.WR_DRV_MP wr_drv_if;
   virtual counter_if.WR_MON_MP wr_mon_if;
   virtual counter_if.RD_MON_MP rd_mon_if; 
                                                                        
   mailbox #(counter_trans) gen2wr = new();
   mailbox #(counter_trans) wr2rm  = new();
   mailbox #(counter_trans) mon2sb  = new();
   mailbox #(counter_trans) rm2sb  = new();
   
   counter_gen        gen_h;
   counter_write_drv  wr_drv_h;
   counter_write_mon  wr_mon_h;
   counter_read_mon   rd_mon_h;
   counter_model      ref_mod_h;
   counter_sb         sb_h;

   function new(virtual counter_if.WR_DRV_MP wr_drv_if,
                virtual counter_if.RD_MON_MP rd_mon_if,
                virtual counter_if.WR_MON_MP wr_mon_if);
      this.wr_drv_if = wr_drv_if;
      this.rd_mon_if = rd_mon_if;
      this.wr_mon_if = wr_mon_if;
   endfunction: new
   
   virtual task build;
      gen_h      = new(gen2wr);
      wr_drv_h   = new(wr_drv_if,gen2wr);
      wr_mon_h   = new(wr_mon_if,wr2rm);
      rd_mon_h   = new(rd_mon_if,mon2sb);
      ref_mod_h  = new(wr2rm, rm2sb);
      sb_h       = new(rm2sb,mon2sb);
   endtask: build

   virtual task start;
      gen_h.start();
      wr_drv_h.start();
      wr_mon_h.start();
      rd_mon_h.start();
      ref_mod_h.start();
      sb_h.start();
   endtask: start

   virtual task stop();
      wait(sb_h.DONE.triggered);
   endtask: stop 

   virtual task run();
      start();
      stop();
      sb_h.report();
   endtask: run

endclass: counter_env

//class test:

class testcase;

   virtual counter_if.WR_DRV_MP wr_drv_if;
   virtual counter_if.WR_MON_MP wr_mon_if;
   virtual counter_if.RD_MON_MP rd_mon_if; 

	counter_env env_h;

   function new(virtual counter_if.WR_DRV_MP wr_drv_if,
		virtual counter_if.WR_MON_MP wr_mon_if,
                virtual counter_if.RD_MON_MP rd_mon_if);
      this.wr_drv_if = wr_drv_if;
      this.wr_mon_if = wr_mon_if;
      this.rd_mon_if = rd_mon_if;
	env_h= new( wr_drv_if,wr_mon_if, rd_mon_if);
   endfunction: new



task build_and_run;
env_h.build();
env_h.run();
$finish;
endtask: build_and_run

endclass: testcase

//class top:

module top();

   import pkg::*;   
   parameter cycle = 10;
   bit clk;

   counter_if DUV_IF(clk);

   testcase test_h;
   
   counter counter(.clk         (clk),
                 .rst    (DUV_IF.rst),
                 .load   (DUV_IF.load),
                 .mode   (DUV_IF.mode),
                 .data_in(DUV_IF.data_in),
                 .data_out(DUV_IF.data_out)); 

   initial
      begin
         clk = 1'b0;
         forever #(cycle/2) clk = ~clk;
      end
   
	initial 
            begin
               test_h = new(DUV_IF,DUV_IF, DUV_IF);
               no_of_transactions = 20;
               test_h.build_and_run();
            end

endmodule : top







      
 


		
