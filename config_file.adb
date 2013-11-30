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
--  config_file.adb	jvinters	6-June-2010
--

pragma License (Modified_GPL);

with Ada.Characters.Handling;		use Ada.Characters.Handling;
with Ada.Strings;			use Ada.Strings;
with Ada.Strings.Fixed;			use Ada.Strings.Fixed;
with Ada.Text_IO;			use Ada.Text_IO;

package body Config_File is

   use type Config_Hash.Cursor;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean
     (This		: in Config_Data;
      Key		: in String;
      Must_Exist	: in Boolean := True;
      Default		: in Boolean := False) return Boolean
   is
      Result		: constant String := Get_String
                            (This, Key, Must_Exist, Boolean'Image (Default));
      Trimmed_Result	: constant String := To_Lower (Trim (Result, Both));
   begin
      if Trimmed_Result = "1" or else Trimmed_Result = "yes"
        or else Trimmed_Result = "true" or else Trimmed_Result = "enabled"
      then
         return True;
      else
         return False;
      end if;
   end Get_Boolean;

   ---------------
   -- Get_Float --
   ---------------

   function Get_Float
     (This		: in Config_Data;
      Key		: in String;
      Must_Exist	: in Boolean := True;
      Default		: in Float := 0.0) return Float
   is
      Result		: constant String := Get_String
                            (This, Key, Must_Exist, Float'Image (Default));
   begin
      return Float'Value (Result);
   end Get_Float;

   -----------------
   -- Get_Integer --
   -----------------
   function Get_Integer
     (This		: in Config_Data;
      Key		: in String;
      Must_Exist	: in Boolean := True;
      Default		: in Integer := 0) return Integer 
   is
       Result : constant String := Get_String
                        (This, Key, Must_Exist, Integer'Image (Default));
   begin
       return Integer'Value (Result);
   end Get_Integer;
   --------------------
   -- Get_Long_Float --
   --------------------

   function Get_Long_Float
     (This		: in Config_Data;
      Key		: in String;
      Must_Exist	: in Boolean := True;
      Default		: in Long_Float := 0.0) return Long_Float
   is
      Result		: constant String := Get_String
                            (This, Key, Must_Exist, Long_Float'Image (Default));
   begin
      return Long_Float'Value (Result);
   end Get_Long_Float;

   -------------------
   -- Get_Read_Only --
   -------------------

   function Get_Read_Only (This : in Config_Data) return Boolean is
   begin
      return This.Read_Only;
   end Get_Read_Only;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (This		: in Config_Data;
      Key		: in String;
      Must_Exist	: in Boolean := True;
      Default		: in String := "") return String
   is
      C			: Config_Hash.Cursor;
      K			: constant Unbounded_String := 
                            To_Unbounded_String (Key);
   begin
      C := Config_Hash.Find (This.Data, K);
      if Config_Hash.Has_Element (C) then
         return To_String (Config_Hash.Element (This.Data, K));
      else
         if Must_Exist then
            raise CONFIG_NOT_FOUND with "key '" & Key & "' not found";
         else
            return Default;
         end if;
      end if;
   end Get_String;

   -------------
   -- Has_Key --
   -------------

   function Has_Key
       (This		: in Config_Data;
        Key		: in String) return Boolean 
   is 
      C			: Config_Hash.Cursor;
      K			: constant Unbounded_String := 
                            To_Unbounded_String (Key);
   begin
      C := Config_Hash.Find (This.Data, K);
      return Config_Hash.Has_Element (C);
   end Has_Key; 
   ----------
   -- Load --
   ----------

   procedure Load
     (This		:    out Config_Data;
      Filename		: in     String;
      Read_Only		: in     Boolean := False)
   is
      File		: File_Type;
      Opened		: Boolean := False;

      function Read_Line (F : in File_Type) return String is
         Last		: Natural;
         Result		: String (1 .. 256);
      begin
         Get_Line (F, Result, Last);
         if Last = Result'Last then
            return Result & Read_Line (F);
         else
            return Result (1 .. Last);
         end if;
      end Read_Line;

   begin
      Config_Hash.Clear (This.Data);
      This.Read_Only := False;

      Open (File, In_File, Filename);
      Opened := True;

      while not End_Of_File (File) loop
         declare
            Key		: Unbounded_String;
            Str		: constant String := Read_Line (File);
            Value	: Unbounded_String;
         begin
            Split_Input (Str, Key, Value);
            Set_Unbounded_String (This, Key, Value);
         end;
      end loop;

      Close (File);
      Opened := False;
      This.Read_Only := Read_Only;

   exception
      when others =>
         if Opened then
            Close (File);
         end if;
         raise CONFIG_IO_ERROR;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save
     (This		: in     Config_Data;
      Filename		: in     String)
   is
      C			: Config_Hash.Cursor;
      File		: File_Type;
      Opened		: Boolean := False;
   begin
      if This.Read_Only then
         raise CONFIG_READ_ONLY with "attempted to save read-only config";
      end if;

      Create (File, Out_File, Filename);
      Opened := True;

      C := Config_Hash.First (This.Data);
      while C /= Config_Hash.No_Element loop
         declare
            Out_Str	: constant String :=
                            To_String (Config_Hash.Key (C)) & '='
                            & Trim (To_String (Config_Hash.Element (C)), Both);
         begin
            Put_Line (File, Out_Str);
            Config_Hash.Next (C);
         end;
      end loop;

      Close (File);
      Opened := False;
   exception
      when others =>
         if Opened then
            Close (File);
         end if;
         raise CONFIG_IO_ERROR;
   end Save;

   -----------------
   -- Set_Boolean --
   -----------------

   procedure Set_Boolean
     (This		: in out Config_Data;
      Key		: in     String;
      Value		: in     Boolean)
   is
   begin
      if Value then
         Set_String (This, Key, "true");
      else
         Set_String (This, Key, "false");
      end if;
   end Set_Boolean;

   ---------------
   -- Set_Float --
   ---------------

   procedure Set_Float
     (This		: in out Config_Data;
      Key		: in     String;
      Value		: in     Float)
   is
   begin
      Set_String (This, Key, Float'Image (Value));
   end Set_Float;

   -----------------
   -- Set_Integer --
   -----------------

   procedure Set_Integer
     (This		: in out Config_Data;
      Key		: in     String;
      Value		: in     Integer)
   is
   begin
      Set_String (This, Key, Integer'Image (Value));
   end Set_Integer;

   --------------------
   -- Set_Long_Float --
   --------------------

   procedure Set_Long_Float
     (This		: in out Config_Data;
      Key		: in     String;
      Value		: in     Long_Float)
   is
   begin
      Set_String (This, Key, Long_Float'Image (Value));
   end Set_Long_Float;

   -------------------
   -- Set_Read_Only --
   -------------------

   procedure Set_Read_Only
     (This		: in out Config_Data;
      Value		: in     Boolean)
   is
   begin
      This.Read_Only := Value;
   end Set_Read_Only;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String
     (This		: in out Config_Data;
      Key		: in     String;
      Value		: in     String)
   is
   begin
      Config_Hash.Include
        (This.Data, 
         To_Unbounded_String (Trim (Key, Both)),
         To_Unbounded_String (Trim (Value, Both)));
   end Set_String;

   --------------------------
   -- Set_Unbounded_String --
   --------------------------

   procedure Set_Unbounded_String
     (This		: in out Config_Data;
      Key		: in     String;
      Value		: in     Ada.Strings.Unbounded.Unbounded_String)
   is
      K			: constant Unbounded_String := To_Unbounded_String (Key);
   begin
      if This.Read_Only then
         raise CONFIG_READ_ONLY with "attempted to write to read-only config";
      end if;

      Config_Hash.Include (This.Data, K, Value);
   end Set_Unbounded_String;


   procedure Set_Unbounded_String
     (This		: in out Config_Data;
      Key		: in     Ada.Strings.Unbounded.Unbounded_String;
      Value		: in     Ada.Strings.Unbounded.Unbounded_String)
   is
   begin
      if This.Read_Only then
         raise CONFIG_READ_ONLY with "attempted to write to read-only config";
      end if;

      Config_Hash.Include (This.Data, Key, Value);
   end Set_Unbounded_String;

   -----------------
   -- Split_Input --
   -----------------

   procedure Split_Input
     (Input		: in     String;
      Key		: in out Unbounded_String;
      Value		: in out Unbounded_String)
   is
      Split_Pos		: Natural;
   begin
      Value := Null_Unbounded_String;

      Split_Pos := Index (Input, "=");
      if Split_Pos = 0 then
         Key := To_Unbounded_String (Input);
         return;
      else
         Set_Unbounded_String (Key, Trim_Nonprint (Input (Input'First .. Split_Pos - 1)));
      end if;

      if Split_Pos < Input'Last then
         Set_Unbounded_String (Value, Trim_Nonprint (Input (Split_Pos + 1 .. Input'Last)));
      end if;
   end Split_Input;

   -------------------
   -- Trim_Nonprint --
   -------------------

   function Trim_Nonprint (This : in String) return String is
      Left		: Natural := This'Last + 1;
      Right		: Natural := This'First;
   begin
      for i in This'Range loop
         if Character'Pos (This (i)) > 32 then
            Left := i;
            exit;
         end if;
      end loop;

      if Left > This'Last then
         return "";
      end if;

      for i in reverse This'Range loop
         if Character'Pos (This (i)) > 32 then
            Right := i;
            exit;
         end if;
      end loop;

      return This (Left .. Right);
   end Trim_Nonprint;

end Config_File;

