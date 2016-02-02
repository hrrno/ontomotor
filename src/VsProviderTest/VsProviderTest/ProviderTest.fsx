


//#r @"../../Samples.HelloWorldTypeProvider/bin/Debug/Samples.HelloWorldTypeProvider.dll"
//let o13 = Samples.HelloWorldTypeProvider.Type13("working")
    

#r @"../../ProtoTypeProvider/bin/Debug/ProtoTypeProvider.dll"
let o1 = Proto.TypeProvider.Type1("one") //Type.ProtoTypeProvider.Type1("one")
o1.InstanceMethod(2)
let d = o1.InstanceProperty
