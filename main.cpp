#include "coreir.h"

using namespace CoreIR;

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

int main() {
  Context* c = newContext();
  Namespace* global = c->getGlobal();

  addCounter(c, global);

  uint pcWidth = 3;

  Type* resetMachineType =
    c->Record({{"clk", c->Named("coreir.clkIn")},
	  {"countOut", c->Array(pcWidth, c->Bit())}});

  Module* resetMachine = global->newModuleDecl("ResetMachine", resetMachineType);
  ModuleDef* def = resetMachine->newModuleDef();

  // Create components of the machine, pc, stage counter, decoder, memory,
  // reset constant, multiplexer
  def->addInstance("pc",
		   "coreir.reg",
		   {{"width", Const(pcWidth)}, {"en", Const(false)}});

  def->addInstance("stageCounter", "global.counter", {{"width", Const(1)}});

  def->addInstance("pcMultiplexer", "coreir.mux", {{"width", Const(pcWidth)}});

  Args wArg({{"width", Const(pcWidth)}});
  def->addInstance("resetConstant", "coreir.const", wArg, {{"value", Const(0)}});

  resetMachine->setDef(def);

  resetMachine->print();
}
