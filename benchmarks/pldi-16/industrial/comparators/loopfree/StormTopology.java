/* ./apache-storm-44e9aaf/storm-core/src/jvm/backtype/storm/generated/StormTopology.java */
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
public class StormTopology implements org.apache.thrift.TBase<StormTopology, StormTopology._Fields>, java.io.Serializable, Cloneable, Comparable<StormTopology> {
  private static final org.apache.thrift.protocol.TStruct STRUCT_DESC = new org.apache.thrift.protocol.TStruct("StormTopology");

  private static final org.apache.thrift.protocol.TField SPOUTS_FIELD_DESC = new org.apache.thrift.protocol.TField("spouts", org.apache.thrift.protocol.TType.MAP, (short)1);
  private static final org.apache.thrift.protocol.TField BOLTS_FIELD_DESC = new org.apache.thrift.protocol.TField("bolts", org.apache.thrift.protocol.TType.MAP, (short)2);
  private static final org.apache.thrift.protocol.TField STATE_SPOUTS_FIELD_DESC = new org.apache.thrift.protocol.TField("state_spouts", org.apache.thrift.protocol.TType.MAP, (short)3);

  private static final Map<Class<? extends IScheme>, SchemeFactory> schemes = new HashMap<Class<? extends IScheme>, SchemeFactory>();
  static {
    schemes.put(StandardScheme.class, new StormTopologyStandardSchemeFactory());
    schemes.put(TupleScheme.class, new StormTopologyTupleSchemeFactory());
  }

  private Map<String,SpoutSpec> spouts; // required
  private Map<String,Bolt> bolts; // required
  private Map<String,StateSpoutSpec> state_spouts; // required

  /** The set of fields this struct contains, along with convenience methods for finding and manipulating them. */
  public enum _Fields implements org.apache.thrift.TFieldIdEnum {
    SPOUTS((short)1, "spouts"),
    BOLTS((short)2, "bolts"),
    STATE_SPOUTS((short)3, "state_spouts");

    private static final Map<String, _Fields> byName = new HashMap<String, _Fields>();

    static {
      for (_Fields field : EnumSet.allOf(_Fields.class)) {
        byName.put(field.getFieldName(), field);
      }
    }

    /**
     * Find the _Fields constant that matches fieldId, or null if its not found.
     */
    public static _Fields findByThriftId(int fieldId) {
      switch(fieldId) {
        case 1: // SPOUTS
          return SPOUTS;
        case 2: // BOLTS
          return BOLTS;
        case 3: // STATE_SPOUTS
          return STATE_SPOUTS;
        default:
          return null;
      }
    }

    /**
     * Find the _Fields constant that matches fieldId, throwing an exception
     * if it is not found.
     */
    public static _Fields findByThriftIdOrThrow(int fieldId) {
      _Fields fields = findByThriftId(fieldId);
      if (fields == null) throw new IllegalArgumentException("Field " + fieldId + " doesn't exist!");
      return fields;
    }

    /**
     * Find the _Fields constant that matches name, or null if its not found.
     */
    public static _Fields findByName(String name) {
      return byName.get(name);
    }

    private final short _thriftId;
    private final String _fieldName;

    _Fields(short thriftId, String fieldName) {
      _thriftId = thriftId;
      _fieldName = fieldName;
    }

    public short getThriftFieldId() {
      return _thriftId;
    }

    public String getFieldName() {
      return _fieldName;
    }
  }

  @Override
  public int compareTo(StormTopology other) {
    if (!getClass().equals(other.getClass())) {
      return getClass().getName().compareTo(other.getClass().getName());
    }

    int lastComparison = 0;

    lastComparison = Boolean.valueOf(is_set_spouts()).compareTo(other.is_set_spouts());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (is_set_spouts()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.spouts, other.spouts);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    lastComparison = Boolean.valueOf(is_set_bolts()).compareTo(other.is_set_bolts());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (is_set_bolts()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.bolts, other.bolts);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    lastComparison = Boolean.valueOf(is_set_state_spouts()).compareTo(other.is_set_state_spouts());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (is_set_state_spouts()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.state_spouts, other.state_spouts);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    return 0;
  }
}
