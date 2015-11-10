/* ./apache-storm-44e9aaf/storm-core/src/jvm/backtype/storm/generated/DistributedRPC.java */
/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
/**
 * Autogenerated by Thrift Compiler (0.9.2)
 *
 * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
 *  @generated
 */
package backtype.storm.generated;

import org.apache.thrift.scheme.IScheme;
import org.apache.thrift.scheme.SchemeFactory;
import org.apache.thrift.scheme.StandardScheme;

import org.apache.thrift.scheme.TupleScheme;
import org.apache.thrift.protocol.TTupleProtocol;
import org.apache.thrift.protocol.TProtocolException;
import org.apache.thrift.EncodingUtils;
import org.apache.thrift.TException;
import org.apache.thrift.async.AsyncMethodCallback;
import org.apache.thrift.server.AbstractNonblockingServer.*;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.EnumMap;
import java.util.Set;
import java.util.HashSet;
import java.util.EnumSet;
import java.util.Collections;
import java.util.BitSet;
import java.nio.ByteBuffer;
import java.util.Arrays;
import javax.annotation.Generated;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@SuppressWarnings({"cast", "rawtypes", "serial", "unchecked"})
@Generated(value = "Autogenerated by Thrift Compiler (0.9.2)", date = "2015-2-6")
public static class execute_args implements org.apache.thrift.TBase<execute_args, execute_args._Fields>, java.io.Serializable, Cloneable, Comparable<execute_args>   {
  boolean is_set_functionName();
  boolean is_set_funcArgs();
  int functionName;
  int funcArgs;
  
  public boolean equals(execute_args o1, execute_args o2) {
    boolean this_present_functionName = true && o1.is_set_functionName();
    boolean that_present_functionName = true && o2.is_set_functionName();
    if (this_present_functionName || that_present_functionName) {
      if (!(this_present_functionName && that_present_functionName)) {
        return false;
      }
      if (equals(o1.functionName, o2.functionName) == 0) {
        return false;
      }
    }
  
    boolean this_present_funcArgs = true && o1.is_set_funcArgs();
    boolean that_present_funcArgs = true && o2.is_set_funcArgs();
    if (this_present_funcArgs || that_present_funcArgs) {
      if (!(this_present_funcArgs && that_present_funcArgs)) {
        return false;
      }
      if (equals(o1.funcArgs, o2.funcArgs) == 0) {
        return false;
      }
    }
  
    return true;
  }


}
