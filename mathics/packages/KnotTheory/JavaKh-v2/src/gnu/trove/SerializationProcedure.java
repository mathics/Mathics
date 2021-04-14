///////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2002, Eric D. Friedman All Rights Reserved.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
///////////////////////////////////////////////////////////////////////////////

package gnu.trove;

import java.io.IOException;
import java.io.ObjectOutput;


/**
 * Implementation of the variously typed procedure interfaces that supports
 * writing the arguments to the procedure out on an ObjectOutputStream.
 * In the case of two-argument procedures, the arguments are written out
 * in the order received.
 *
 * <p>
 * Any IOException is trapped here so that it can be rethrown in a writeObject
 * method.
 * </p>
 *
 * Created: Sun Jul  7 00:14:18 2002
 *
 * @author Eric D. Friedman
 * @version $Id: SerializationProcedure.java,v 1.5 2006/11/10 23:27:54 robeden Exp $
 */

class SerializationProcedure implements /* TDoubleDoubleProcedure,
    TDoubleFloatProcedure,
    TDoubleIntProcedure,
    TDoubleLongProcedure,
    TDoubleShortProcedure,
    TDoubleByteProcedure,
    TDoubleObjectProcedure,
    TDoubleProcedure,
    TFloatDoubleProcedure,
    TFloatFloatProcedure,
    TFloatIntProcedure,
    TFloatLongProcedure,
    TFloatShortProcedure,
    TFloatByteProcedure,
    TFloatObjectProcedure,
    TFloatProcedure,
    TIntDoubleProcedure,
    TIntFloatProcedure,
    TIntIntProcedure,
    TIntLongProcedure,
    TIntShortProcedure,
    TIntByteProcedure,
    TIntObjectProcedure,
    TIntProcedure,
    TLongDoubleProcedure,
    TLongFloatProcedure,
    TLongIntProcedure,
    TLongLongProcedure,
    TLongShortProcedure,
    TLongByteProcedure,
    TLongObjectProcedure,
    TLongProcedure,
    TShortDoubleProcedure,
    TShortFloatProcedure,
    TShortIntProcedure,
    TShortLongProcedure,
    TShortShortProcedure,
    TShortByteProcedure,
    TShortObjectProcedure,
    TShortProcedure,
    TByteDoubleProcedure,
    TByteFloatProcedure,
    TByteIntProcedure,
    TByteLongProcedure,
    TByteShortProcedure,
    TByteByteProcedure,
    TByteObjectProcedure,
    TByteProcedure,
    TObjectDoubleProcedure,
    TObjectFloatProcedure,
    TObjectIntProcedure,
    TObjectLongProcedure,
    TObjectShortProcedure,
    TObjectByteProcedure,
    TObjectObjectProcedure, */
    TObjectProcedure {

    private final ObjectOutput stream;
    IOException exception;

    SerializationProcedure ( ObjectOutput stream) {
        this.stream = stream;
    }

    public boolean execute(byte val) {
        try {
            stream.writeByte(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(short val) {
        try {
            stream.writeShort(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(int val) {
        try {
            stream.writeInt(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(double val) {
        try {
            stream.writeDouble(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(long val) {
        try {
            stream.writeLong(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(float val) {
        try {
            stream.writeFloat(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(Object val) {
        try {
            stream.writeObject(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(Object key, Object val) {
        try {
            stream.writeObject(key);
            stream.writeObject(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(Object key, byte val) {
        try {
            stream.writeObject(key);
            stream.writeByte(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(Object key, short val) {
        try {
            stream.writeObject(key);
            stream.writeShort(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(Object key, int val) {
        try {
            stream.writeObject(key);
            stream.writeInt(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(Object key, long val) {
        try {
            stream.writeObject(key);
            stream.writeLong(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(Object key, double val) {
        try {
            stream.writeObject(key);
            stream.writeDouble(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(Object key, float val) {
        try {
            stream.writeObject(key);
            stream.writeFloat(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(int key, byte val) {
        try {
            stream.writeInt(key);
            stream.writeByte(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(int key, short val) {
        try {
            stream.writeInt(key);
            stream.writeShort(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(int key, Object val) {
        try {
            stream.writeInt(key);
            stream.writeObject(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(int key, int val) {
        try {
            stream.writeInt(key);
            stream.writeInt(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(int key, long val) {
        try {
            stream.writeInt(key);
            stream.writeLong(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(int key, double val) {
        try {
            stream.writeInt(key);
            stream.writeDouble(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(int key, float val) {
        try {
            stream.writeInt(key);
            stream.writeFloat(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(long key, Object val) {
        try {
            stream.writeLong(key);
            stream.writeObject(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(long key, byte val) {
        try {
            stream.writeLong(key);
            stream.writeByte(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(long key, short val) {
        try {
            stream.writeLong(key);
            stream.writeShort(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(long key, int val) {
        try {
            stream.writeLong(key);
            stream.writeInt(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(long key, long val) {
        try {
            stream.writeLong(key);
            stream.writeLong(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(long key, double val) {
        try {
            stream.writeLong(key);
            stream.writeDouble(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(long key, float val) {
        try {
            stream.writeLong(key);
            stream.writeFloat(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(double key, Object val) {
        try {
            stream.writeDouble(key);
            stream.writeObject(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(double key, byte val) {
        try {
            stream.writeDouble(key);
            stream.writeByte(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(double key, short val) {
        try {
            stream.writeDouble(key);
            stream.writeShort(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(double key, int val) {
        try {
            stream.writeDouble(key);
            stream.writeInt(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(double key, long val) {
        try {
            stream.writeDouble(key);
            stream.writeLong(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(double key, double val) {
        try {
            stream.writeDouble(key);
            stream.writeDouble(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(double key, float val) {
        try {
            stream.writeDouble(key);
            stream.writeFloat(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(float key, Object val) {
        try {
            stream.writeFloat(key);
            stream.writeObject(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(float key, byte val) {
        try {
            stream.writeFloat(key);
            stream.writeByte(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(float key, short val) {
        try {
            stream.writeFloat(key);
            stream.writeShort(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(float key, int val) {
        try {
            stream.writeFloat(key);
            stream.writeInt(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(float key, long val) {
        try {
            stream.writeFloat(key);
            stream.writeLong(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(float key, double val) {
        try {
            stream.writeFloat(key);
            stream.writeDouble(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(float key, float val) {
        try {
            stream.writeFloat(key);
            stream.writeFloat(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(byte key, Object val) {
        try {
            stream.writeByte(key);
            stream.writeObject(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(byte key, byte val) {
        try {
            stream.writeByte(key);
            stream.writeByte(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(byte key, short val) {
        try {
            stream.writeByte(key);
            stream.writeShort(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(byte key, int val) {
        try {
            stream.writeByte(key);
            stream.writeInt(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(byte key, long val) {
        try {
            stream.writeByte(key);
            stream.writeLong(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(byte key, double val) {
        try {
            stream.writeByte(key);
            stream.writeDouble(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(byte key, float val) {
        try {
            stream.writeByte(key);
            stream.writeFloat(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(short key, Object val) {
        try {
            stream.writeShort(key);
            stream.writeObject(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(short key, byte val) {
        try {
            stream.writeShort(key);
            stream.writeByte(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(short key, short val) {
        try {
            stream.writeShort(key);
            stream.writeShort(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(short key, int val) {
        try {
            stream.writeShort(key);
            stream.writeInt(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(short key, long val) {
        try {
            stream.writeShort(key);
            stream.writeLong(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(short key, double val) {
        try {
            stream.writeShort(key);
            stream.writeDouble(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(short key, float val) {
        try {
            stream.writeShort(key);
            stream.writeFloat(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }
}// SerializationProcedure
