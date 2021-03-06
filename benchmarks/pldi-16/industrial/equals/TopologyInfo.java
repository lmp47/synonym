/* ./apache-storm-44e9aaf/storm-core/src/jvm/backtype/storm/generated/TopologyInfo.java */
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
public class TopologyInfo implements org.apache.thrift.TBase<TopologyInfo, TopologyInfo._Fields>, java.io.Serializable, Cloneable, Comparable<TopologyInfo> {
  
  private String id; // required
  private String name; // required
  private int uptime_secs; // required
  private ListExecutorSummary executors; // required
  private String status; // required
  private MapStringListErrorInfo errors; // required
  private String sched_status; // optional
  private String owner; // optional

  boolean is_set_id();
  boolean is_set_name();
  boolean is_set_executors();
  boolean is_set_status();
  boolean is_set_errors();
  boolean is_set_sched_status();
  boolean is_set_owner();

  public boolean equals(TopologyInfo o1, TopologyInfo o2) {
    boolean this_present_id = true && o1.is_set_id();
    boolean that_present_id = true && o2.is_set_id();
    if (this_present_id || that_present_id) {
      if (!(this_present_id && that_present_id))
        return false;
      if (equals(o1.id,o2.id) == 0)
        return false;
    }

    boolean this_present_name = true && o1.is_set_name();
    boolean that_present_name = true && o2.is_set_name();
    if (this_present_name || that_present_name) {
      if (!(this_present_name && that_present_name))
        return false;
      if (equals(o1.name,o2.name) == 0)
        return false;
    }

    boolean this_present_uptime_secs = true;
    boolean that_present_uptime_secs = true;
    if (this_present_uptime_secs || that_present_uptime_secs) {
      if (!(this_present_uptime_secs && that_present_uptime_secs))
        return false;
      if (o1.uptime_secs != o2.uptime_secs)
        return false;
    }

    boolean this_present_executors = true && o1.is_set_executors();
    boolean that_present_executors = true && o2.is_set_executors();
    if (this_present_executors || that_present_executors) {
      if (!(this_present_executors && that_present_executors))
        return false;
      if (equals(o1.executors, o2.executors) == 0)
        return false;
    }

    boolean this_present_status = true && o1.is_set_status();
    boolean that_present_status = true && o2.is_set_status();
    if (this_present_status || that_present_status) {
      if (!(this_present_status && that_present_status))
        return false;
      if (equals(o1.status, o2.status) == 0)
        return false;
    }

    boolean this_present_errors = true && o1.is_set_errors();
    boolean that_present_errors = true && o2.is_set_errors();
    if (this_present_errors || that_present_errors) {
      if (!(this_present_errors && that_present_errors))
        return false;
      if (equals(o1.errors, o2.errors) == 0)
        return false;
    }

    boolean this_present_sched_status = true && o1.is_set_sched_status();
    boolean that_present_sched_status = true && o2.is_set_sched_status();
    if (this_present_sched_status || that_present_sched_status) {
      if (!(this_present_sched_status && that_present_sched_status))
        return false;
      if (equals(o1.sched_status, o2.sched_status) == 0)
        return false;
    }

    boolean this_present_owner = true && o1.is_set_owner();
    boolean that_present_owner = true && o2.is_set_owner();
    if (this_present_owner || that_present_owner) {
      if (!(this_present_owner && that_present_owner))
        return false;
      if (equals(o1.owner, o2.owner) == 0)
        return false;
    }

    return true;
  }

}

