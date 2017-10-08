#define CATCH_CONFIG_MAIN

#include "catch.hpp"

#include "coreir.h"
#include "coreir/simulator/interpreter.h"
#include "coreir/passes/transform/rungenerators.h"

using namespace CoreIR;
using namespace CoreIR::Passes;
using namespace std;

void addIncReset(Context* c, Namespace* global) {
  Params incResetParams({{"width", AINT}});

  TypeGen* incResetTypeGen =
    global->newTypeGen("IncResetTypeGen",
		       incResetParams,
		       [](Context* c, Args args) {
			 uint width = args.at("width")->get<int>();

			 return c->Record({
			     {"selectBit", c->BitIn()},
			       {"in", c->Array(width, c->BitIn())},
				 {"out", c->Array(width, c->Bit())}
			   });
		       }
		       );

  Generator* inc = global->newGeneratorDecl("incReset",
					    incResetTypeGen,
					    incResetParams);

  inc->setGeneratorDefFromFun([](ModuleDef* def,Context* c, Type* t, Args args) {
    uint width = args.at("width")->get<int>();
      
    def->addInstance("pcMultiplexer", "coreir.mux", {{"width", Const(width)}});
    def->addInstance("incrementer", "global.inc", {{"width", Const(width)}});
    Args wArg({{"width", Const(width)}});
    def->addInstance("resetConstant", "coreir.const", wArg, {{"value", Const(0)}});

    // Connections
    def->connect("self.selectBit", "pcMultiplexer.sel");
    def->connect("self.in", "incrementer.in");

    def->connect("incrementer.out", "pcMultiplexer.in0");
    def->connect("resetConstant.out", "pcMultiplexer.in1");

    def->connect("pcMultiplexer.out", "self.out");
    
  });
  
}

void addIncrementer(Context* c, Namespace* global) {

  Params incParams({{"width", AINT}});
  //Other param types: ABOOL,ASTRING,ATYPE

  TypeGen* incTypeGen = global->newTypeGen(
    "IncTypeGen", //name of typegen
    incParams, //Params required for typegen
    [](Context* c, Args args) { //lambda for generating the type

      uint width = args.at("width")->get<int>();

      return c->Record({
	  {"in", c->Array(width, c->BitIn())},
	    {"out",c->Array(width,c->Bit())},
      });
    } //end lambda
  ); //end newTypeGen

  ASSERT(global->hasTypeGen("IncTypeGen"),"Can check for typegens in namespaces");


  //Now lets create a generator declaration for our inc
  Generator* inc = global->newGeneratorDecl("inc", incTypeGen, incParams);
  //The third argument is the inc parameters. This needs to be a superset of the parameters for the typegen.
  
  //Now lets define our generator function. I am going to use a lambda again, but you could pass in
  //  a normal function with the same type signature.
  inc->setGeneratorDefFromFun([](ModuleDef* def,Context* c, Type* t, Args args) {
    //Similar to the typegen, lets extract the width;
    uint width = args.at("width")->get<int>();
      
    //Now just define the inc with with all the '16's replaced by 'width'
    Args wArg({{"width", Const(width)}});
    def->addInstance("ai","coreir.add",wArg);
    def->addInstance("ci","coreir.const",wArg,{{"value", Const(1)}});
    
    //Connections
    def->connect("ci.out","ai.in0");
    def->connect("self.in","ai.in1");
    def->connect("ai.out","self.out");
  }); //end lambda, end function

}

void addCounter(Context* c, Namespace* global) {
  //Now lets make our counter as a generator.
  //We want our Generator to be able to take in the parameter width.
  //We need to specify that the width is of type "int"
  Params counterParams({{"width",AINT}}); //"Arg Int". I know, its bad
  //Other param types: ABOOL,ASTRING,ATYPE

  //Instead of defining a type, now we need to define a type Generator. This allows CoreIR to statically type check all connections.
  TypeGen* counterTypeGen = global->newTypeGen(
    "CounterTypeGen", //name of typegen
    counterParams, //Params required for typegen
    [](Context* c, Args args) { //lambda for generating the type
      //Arg* widthArg = args.at("width"); //Checking for valid args is already done for you
      uint width = args.at("width")->get<int>(); //widthArg->get<int>(); //get function to extract the arg value.
      return c->Record({
        {"en",c->BitIn()}, 
        {"out",c->Array(width,c->Bit())}, //Note: Array is parameterized by width now
        {"clk",c->Named("coreir.clkIn")},
      });
    } //end lambda
  ); //end newTypeGen
  ASSERT(global->hasTypeGen("CounterTypeGen"),"Can check for typegens in namespaces");


  //Now lets create a generator declaration for our counter
  Generator* counter = global->newGeneratorDecl("counter",counterTypeGen,counterParams);
  //The third argument is the counter parameters. This needs to be a superset of the parameters for the typegen.
  
  //Now lets define our generator function. I am going to use a lambda again, but you could pass in
  //  a normal function with the same type signature.
  counter->setGeneratorDefFromFun([](ModuleDef* def,Context* c, Type* t, Args args) {
    //ModuleDef* def : The circuit you are defining.
    //Type* t: The generated Type of the counter (Using your counterTypeGen function!)
    //Args args: The arguments supplied to the instance of the counter.
    
    //Similar to the typegen, lets extract the width;
    uint width = args.at("width")->get<int>();
      
    //Now just define the counter with with all the '16's replaced by 'width'
    Args wArg({{"width", Const(width)}});
    def->addInstance("ai","coreir.add",wArg);
    def->addInstance("ci","coreir.const",wArg,{{"value", Const(1)}});
    //Reg has default arguments. en/clr/rst are False by default. Init is also 0 by default
    def->addInstance("ri","coreir.reg",{{"width", Const(width)},{"en", Const(true)}});
    
    //Connections
    def->connect("self.clk","ri.clk");
    def->connect("self.en","ri.en");
    def->connect("ci.out","ai.in0");
    def->connect("ai.out","ri.in");
    def->connect("ri.out","ai.in1");
    def->connect("ri.out","self.out");
  }); //end lambda, end function
  
}

TEST_CASE("Counter") {
  Context* c = newContext();
  Namespace* global = c->getGlobal();

  addCounter(c, global);

  uint pcWidth = 17;
  Type* counterTestType =
    c->Record({
	{"en", c->BitIn()},
	  {"clk", c->Named("coreir.clkIn")},
	    {"counterOut", c->Array(pcWidth, c->Bit())}});

  Module* counterTest = global->newModuleDecl("counterMod", counterTestType);
  ModuleDef* def = counterTest->newModuleDef();

  def->addInstance("counter", "global.counter", {{"width", Const(pcWidth)}});

  def->connect("self.en", "counter.en");
  def->connect("self.clk", "counter.clk");
  def->connect("counter.out", "self.counterOut");

  counterTest->setDef(def);

  RunGenerators rg;
  rg.runOnNamespace(global);

  // Inline increment
  inlineInstance(def->getInstances()["counter"]);

  SimulatorState state(counterTest);
  state.setValue("counter$ri.out", BitVec(pcWidth, 400));
  state.setValue("self.en", BitVec(1, 1));
  state.setClock("self.clk", 0, 1);

  state.execute();

  REQUIRE(state.getBitVec("self.counterOut") == BitVec(pcWidth, 400));

  state.setValue("counter$ri.out", BitVec(pcWidth, 400));
  state.setValue("self.en", BitVec(1, 0));
  state.setClock("self.clk", 0, 1);
  
  state.execute();
  state.execute();
  state.execute();
  state.execute();

  cout << "Output = " << state.getBitVec("self.counterOut") << endl;

  REQUIRE(state.getBitVec("self.counterOut") == BitVec(pcWidth, 400));
  
}

TEST_CASE("Incrementer") {
  Context* c = newContext();
  Namespace* global = c->getGlobal();

  addIncrementer(c, global);

  uint pcWidth = 3;

  Type* incTestType =
    c->Record({{"incIn", c->Array(pcWidth, c->BitIn())},
	{"incOut", c->Array(pcWidth, c->Bit())}});

  Module* incTest = global->newModuleDecl("incMod", incTestType);
  ModuleDef* def = incTest->newModuleDef();


  def->addInstance("incrementer", "global.inc", {{"width", Const(pcWidth)}});

  def->connect("self.incIn", "incrementer.in");
  def->connect("incrementer.out", "self.incOut");

  incTest->setDef(def);

  RunGenerators rg;
  rg.runOnNamespace(global);

  // Inline increment
  inlineInstance(def->getInstances()["incrementer"]);

  SimulatorState state(incTest);
  state.setValue("self.incIn", BitVec(pcWidth, 0));

  state.execute();

  REQUIRE(state.getBitVec("self.incOut") == BitVec(pcWidth, 1));

  state.setValue("self.incIn", BitVec(pcWidth, (1 << 2) | (1 << 1) | 1));

  state.execute();

  cout << "Output = " << state.getBitVec("self.incOut") << endl;

  REQUIRE(state.getBitVec("self.incOut") == BitVec(pcWidth, 0));

}

TEST_CASE("Increment or reset") {
  Context* c = newContext();
  Namespace* global = c->getGlobal();

  addIncrementer(c, global);
  addIncReset(c, global);

  uint width = 62;

  Type* irType =
    c->Record({
	{"in", c->Array(width, c->BitIn())},
	  {"out", c->Array(width, c->Bit())},
	    {"reset", c->BitIn()}
      });

  Module* irM = global->newModuleDecl("irM", irType);
  ModuleDef* def = irM->newModuleDef();

  def->addInstance("ir", "global.incReset", {{"width", Const(width)}});

  def->connect("self.in", "ir.in");
  def->connect("self.reset", "ir.selectBit");
  def->connect("ir.out", "self.out");

  irM->setDef(def);

  RunGenerators rg;
  rg.runOnNamespace(global);

  // Inline increment
  inlineInstance(def->getInstances()["ir"]);
  inlineInstance(def->getInstances()["ir$incrementer"]);

  cout << "Checking saving and loading pregen" << endl;
  if (!saveToFile(global, "incReset.json", irM)) {
    cout << "Could not save to json!!" << endl;
    c->die();
  }

  SimulatorState state(irM);

  SECTION("Reset") {
    state.setValue("self.in", BitVec(width, 23));
    state.setValue("self.reset", BitVec(1, 1));

    state.execute();

    REQUIRE(state.getBitVec("self.out") == BitVec(width, 0));

  }

  SECTION("No reset") {
    state.setValue("self.in", BitVec(width, 236));
    state.setValue("self.reset", BitVec(1, 0));

    state.execute();

    REQUIRE(state.getBitVec("self.out") == BitVec(width, 237));

  }

}

TEST_CASE("Enabled memory") {
  
}

TEST_CASE("Full machine build") {
  Context* c = newContext();
  Namespace* global = c->getGlobal();

  addCounter(c, global);
  addIncrementer(c, global);
  addIncReset(c, global);

  uint pcWidth = 3;
  uint memDepth = pow(2, pcWidth);
  uint memWidth = 1;

  Type* resetMachineType =
    c->Record({
	{"clk", c->Named("coreir.clkIn")},
	  {"countOut", c->Array(pcWidth, c->Bit())},
	    {"dummyWAddr", c->Array(pcWidth, c->BitIn())},
	      {"dummyWData", c->Array(memWidth, c->BitIn())},
		{"dummyWen", c->BitIn()}});

  Module* resetMachine = global->newModuleDecl("ResetMachine", resetMachineType);
  ModuleDef* def = resetMachine->newModuleDef();

  // Create components of the machine, pc, stage counter, decoder, memory,
  // reset constant, multiplexer
  def->addInstance("pc",
		   "coreir.reg",
		   {{"width", Const(pcWidth)}, {"en", Const(true)}});

  def->addInstance("stageCounter", "global.counter", {{"width", Const(1)}});

  def->addInstance("counterAndr", "coreir.andr", {{"width", Const(1)}});

  // Creating memory that is always enabled

  def->addInstance("mainMem",
		   "coreir.mem",
		   {{"width", Const(memWidth)},{"depth", Const(memDepth)}},
		   {{"init", Const("0")}});

  def->addInstance("incR", "global.incReset", {{"width", Const(pcWidth)}});

  // Connect machine components

  // Connect dummy memory ports
  def->connect("self.dummyWAddr", "mainMem.waddr");
  def->connect("self.dummyWData", "mainMem.wdata");
  def->connect("self.dummyWen", "mainMem.wen");

  // Connect clock to sequential elements
  def->connect("self.clk", "stageCounter.clk");
  def->connect("self.clk", "pc.clk");
  def->connect("self.clk", "mainMem.clk");


  // Connect stage counter to pc
  // TODO: Add andr node to eliminate this hack
  def->connect("stageCounter.out", "counterAndr.in");
  def->connect("counterAndr.out", "pc.en");

  // Connect pc to memory and reset counter
  def->connect("pc.out", "mainMem.raddr");
  def->connect("pc.out", "incR.in");

  // Memory output to incReset
  def->addInstance("rdataAndr", "coreir.andr", {{"width", Const(1)}});
  def->connect("mainMem.rdata", "rdataAndr.in");
  def->connect("rdataAndr.out", "incR.selectBit");

  def->connect("incR.out", "pc.in");

  // Set definition and save
  resetMachine->setDef(def);

  // Simulate
  SimulatorState state(resetMachine);
  state.setMemory("mainMem", BitVec(3, 0), BitVec(1, 0));
  state.setMemory("mainMem", BitVec(3, 1), BitVec(1, 0));
  state.setMemory("mainMem", BitVec(3, 2), BitVec(1, 1));
  
  resetMachine->print();

  cout << "Checking saving and loading pregen" << endl;
  if (!saveToFile(global, "resetMachine.json", resetMachine)) {
    cout << "Could not save to json!!" << endl;
    c->die();
  }
}
