--
--  Copyright (C) 2010 John Vinters
--
--  This library is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 3 of the License, or
--  (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with this program; if not, see <http://www.gnu.org/licenses/>.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--
--  config_file.ads	jvinters	6-June-2010
--

pragma License (Modified_GPL);

private with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
private with Ada.Strings.Unbounded.Hash;

package Config_File is

   CONFIG_IO_ERROR	: Exception;	--  problem loading or saving config
   CONFIG_NOT_FOUND	: Exception;	--  key not found
   CONFIG_READ_ONLY	: Exception;	--  attempt to write to read-only config

   type Config_Data is private;

   function Get_Boolean
     (This		: in Config_Data;
      Key		: in String;
      Must_Exist	: in Boolean := True;
      Default		: in Boolean := False) return Boolean;
   --  Looks up a Boolean value.
   --  If Must_Exist is False, then Default is returned if the Key can't be
   --  found.  If Must_Exist is true then CONFIG_NOT_FOUND is raised if the
   --  Key can't be found.

   function Get_Float
     (This		: in Config_Data;
      Key		: in String;
      Must_Exist	: in Boolean := True;
      Default		: in Float := 0.0) return Float;
   --  Looks up a Float value.
   --  If Must_Exist is False, then Default is returned if the Key can't be
   --  found.  If Must_Exist is true then CONFIG_NOT_FOUND is raised if the
   --  Key can't be found.
   --  CONSTRAINT_ERROR may be raised if the value stored against the key
   --  isn't a valid Float.

   function Get_Integer
     (This		: in Config_Data;
      Key		: in String;
      Must_Exist	: in Boolean := True;
      Default		: in Integer := 0) return Integer;
   --  Looks up a Integer value.
   --  If Must_Exist is False, then Default is returned if the Key can't be
   --  found.  If Must_Exist is true then CONFIG_NOT_FOUND is raised if the
   --  Key can't be found.
   --  CONSTRAINT_ERROR may be raised if the value stored against the key
   --  isn't a valid Integer 
   
   function Get_Long_Float
     (This		: in Config_Data;
      Key		: in String;
      Must_Exist	: in Boolean := True;
      Default		: in Long_Float := 0.0) return Long_Float;
   --  Looks up a Long_Float value.
   --  If Must_Exist is False, then Default is returned if the Key can't be
   --  found.  If Must_Exist is true then CONFIG_NOT_FOUND is raised if the
   --  Key can't be found.
   --  CONSTRAINT_ERROR may be raised if the value stored against the key
   --  isn't a valid Long_Float.

   function Get_Read_Only (This : in Config_Data) return Boolean;
   --  Returns the Read_Only status of the configuration.

   function Get_String
     (This		: in Config_Data;
      Key		: in String;
      Must_Exist	: in Boolean := True;
      Default		: in String := "") return String;
   --  Looks up a string value.
   --  If Must_Exist is False, then Default is returned if the Key can't be
   --  found.  If Must_Exist is true then CONFIG_NOT_FOUND is raised if the
   --  Key can't be found.

   function Has_Key
       (This		: in Config_Data; 
        Key		: in String) return Boolean; 
   --  Returns False if Key can't be found.  

   procedure Load
     (This		:    out Config_Data;
      Filename		: in     String;
      Read_Only		: in     Boolean := False);
   --  Loads the configuration file from disk.
   --  If Read_Only is true, then attempts to write to the configuration file
   --  will result in CONFIG_READ_ONLY.
   --  If the file can't be loaded, CONFIG_IO_ERROR will be raised.

   procedure Save
     (This		: in     Config_Data;
      Filename		: in     String);
   --  Saves the configuration file to disk.
   --  If the file has been opened Read_Only, then CONFIG_READ_ONLY will be
   --  raised.
   --  If the file can't be saved, CONFIG_IO_ERROR will be raised.

   procedure Set_Boolean
     (This		: in out Config_Data;
      Key		: in     String;
      Value		: in     Boolean);
   --  Stores a Boolean Key/Value pair.  CONFIG_READ_ONLY will be raised if
   --  the configuration is read only.

   procedure Set_Float
     (This		: in out Config_Data;
      Key		: in     String;
      Value		: in     Float);
   --  Stores a Float Key/Value pair.  CONFIG_READ_ONLY will be raised if
   --  the configuration is read only.

   procedure Set_Integer
     (This		: in out Config_Data;
      Key		: in     String;
      Value		: in     Integer);
   --  Stores an Integer Key/Value pair.  CONFIG_READ_ONLY will be raised if
   --  the configuration is read only.

   procedure Set_Long_Float
     (This		: in out Config_Data;
      Key		: in     String;
      Value		: in     Long_Float);
   --  Stores a Long_Float Key/Value pair.  CONFIG_READ_ONLY will be raised if
   --  the configuration is read only.

   procedure Set_Read_Only
     (This		: in out Config_Data;
      Value		: in     Boolean);
   --  Sets/Clears the Read_Only flag.

   procedure Set_String
     (This		: in out Config_Data;
      Key		: in     String;
      Value		: in     String);
   --  Stores a String Key/Value pair.  CONFIG_READ_ONLY will be raised if the
   --  configuration is read only.

   procedure Set_Unbounded_String
     (This		: in out Config_Data;
      Key		: in     String;
      Value		: in     Ada.Strings.Unbounded.Unbounded_String);

   procedure Set_Unbounded_String
     (This		: in out Config_Data;
      Key		: in     Ada.Strings.Unbounded.Unbounded_String;
      Value		: in     Ada.Strings.Unbounded.Unbounded_String);
   --  Stores a Unbounded_String Key/Value pair.  CONFIG_READ_ONLY will be 
   --  raised if the configuration is read only.

private

   use Ada.Strings.Unbounded;

   package Config_Hash is new Ada.Containers.Hashed_Maps 
     (Key_Type		=> Ada.Strings.Unbounded.Unbounded_String,
      Element_Type	=> Ada.Strings.Unbounded.Unbounded_String,
      Hash		=> Ada.Strings.Unbounded.Hash,
      Equivalent_Keys	=> Ada.Strings.Unbounded."=",
      "="		=> Ada.Strings.Unbounded."=");

   type Config_Data is record
      Data		: Config_Hash.Map;
      Read_Only		: Boolean := False;
   end record;

   procedure Split_Input
     (Input		: in     String;
      Key		: in out Unbounded_String;
      Value		: in out Unbounded_String);
   --  Splits an input line into key/value, discarding the '=' separator (if any)

   function Trim_Nonprint (This : in String) return String;
   --  Trims non-printing and whitespace characters from beginning and end
   --  of string.

end Config_File;

