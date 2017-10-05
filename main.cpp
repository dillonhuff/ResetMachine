#define CATCH_CONFIG_MAIN

#include "catch.hpp"

#include "coreir.h"
#include "coreir/simulator/interpreter.h"
#include "coreir/passes/transform/rungenerators.h"

using namespace CoreIR;
using namespace CoreIR::Passes;
using namespace std;

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

  REQUIRE(state.getBitVec("self.counterOut") == BitVec(pcWidth, 401));

  state.setValue("counter$ri.out", BitVec(pcWidth, 400));
  state.setValue("self.en", BitVec(1, 1));
  state.setClock("self.clk", 0, 1);
  
  state.execute();

  cout << "Output = " << state.getBitVec("self.counterOut") << endl;

  REQUIRE(state.getBitVec("self.counterOut") == BitVec(pcWidth, 24));

  state.setValue("counter$ri.out", BitVec(pcWidth, 400));
  state.setValue("self.en", BitVec(1, 1));
  state.setClock("self.clk", 1, 0);
  
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

TEST_CASE("Full machine build") {
  Context* c = newContext();
  Namespace* global = c->getGlobal();

  addCounter(c, global);
  addIncrementer(c, global);

  uint pcWidth = 3;
  uint memDepth = pow(2, pcWidth);
  uint memWidth = 1;

  Type* resetMachineType =
    c->Record({{"clk", c->Named("coreir.clkIn")},
	  {"countOut", c->Array(pcWidth, c->Bit())}});

  Module* resetMachine = global->newModuleDecl("ResetMachine", resetMachineType);
  ModuleDef* def = resetMachine->newModuleDef();

  // Create components of the machine, pc, stage counter, decoder, memory,
  // reset constant, multiplexer
  def->addInstance("pc",
		   "coreir.reg",
		   {{"width", Const(pcWidth)}, {"en", Const(true)}});

  def->addInstance("stageCounter", "global.counter", {{"width", Const(1)}});

  def->addInstance("pcMultiplexer", "coreir.mux", {{"width", Const(pcWidth)}});

  Args wArg({{"width", Const(pcWidth)}});
  def->addInstance("resetConstant", "coreir.const", wArg, {{"value", Const(0)}});

  def->addInstance("mainMem",
		   "coreir.mem",
		   {{"width", Const(memWidth)},{"depth", Const(memDepth)}},
		   {{"init", Const("0")}});

  def->addInstance("incrementer", "global.inc", {{"width", Const(pcWidth)}});

  resetMachine->setDef(def);

  resetMachine->print();

  cout << "Checking saving and loading pregen" << endl;
  if (!saveToFile(global, "resetMachine.json", resetMachine)) {
    cout << "Could not save to json!!" << endl;
    c->die();
  }
}
