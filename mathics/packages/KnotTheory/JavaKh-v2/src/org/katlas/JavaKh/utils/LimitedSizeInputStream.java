package org.katlas.JavaKh.utils;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * An <code>InputStream</code> which reads only up to a specific maximum 
number of bytes.
 * 
 * "Borrowed" from http://mail-archives.apache.org/mod_mbox/hc-dev/200310.mbox/%3C200310101247.40166.ck@rrzn.uni-hannover.de%3E
 * 
 * @author Christian Kohlschütter
 * @version $Id$
 */
public class LimitedSizeInputStream extends FilterInputStream {
    private long max;
    private long count = 0;
    private boolean pastEOF = false;
    private long mark = 0;

    /**
     * Creates a new <code>LimitedSizeInputStream</code> using the given
     * underlying <code>InputStream</code>, reading a maximum of 
<code>maxBytes</code>
     * bytes from that stream.
     */
    public LimitedSizeInputStream(InputStream in, long maxBytes) {
        super(in);
        setLimit(maxBytes);
        resetByteCounter();
    }
    
    /**
     * Returns the current limit.
     * 
     * @return  Maximum number of bytes to be read.
     */
    public long getLimit() {
        return max;
    }
    
    /**
     * Sets the new limit.
     * 
     * @param   maxBytes  Maximum number of bytes to be read.
     */
    public void setLimit(long maxBytes) {
        long oldMax = max;
        this.max = Math.max(0,maxBytes);
        if(max < oldMax) {
            pastEOF = true;
        }
    }
    
    /**
     * Returns the number of bytes read/skipped.
     * 
     * @return  Number of consumed bytes.
     */
    public long getByteCounter() {
        return count;
    }
    
    /**
     * Resets the read-bytes counter to 0.
     */
    public void resetByteCounter() {
        count = 0;
        mark = 0;
        pastEOF = false;
    }

    /* (non-Javadoc)
     * @see java.io.InputStream#read()
     */
    public int read() throws IOException {
        if(pastEOF) {
            return -1;
        }
        if(max != 0 && count >= max) {
            pastEOF = true;
            return -1;
        }
        
        int b = in.read();
        if(b >= 0) {
            count++;
        } else {
            pastEOF = true;
        }
        return b;
    }

    /* (non-Javadoc)
     * @see java.io.InputStream#available()
     */
    public int available() throws IOException {
        if(max != 0 && count >= max) {
            return 0;
        }
        return Math.min(in.available(), (int)(max-count));
    }

    /* (non-Javadoc)
     * @see java.io.InputStream#close()
     */
    public void close() throws IOException {
        in.close();
    }

    /* (non-Javadoc)
     * @see java.io.InputStream#mark(int)
     */
    public synchronized void mark(int readlimit) {
        in.mark(readlimit);
        mark = count;
    }

    /* (non-Javadoc)
     * @see java.io.InputStream#read(byte[], int, int)
     */
    public int read(byte[] b, int off, int len) throws IOException {
        if(pastEOF) {
            return -1;
        }
        if(max != 0) {
            if(count >= max) {
                return -1;
            }
            len = Math.min(len, (int)(max-count));
        }
        if(len <= 0) {
            return 0;
        }

        int r = in.read(b, off, len);
        count += r;
        if(count >= max || r == -1) {
            pastEOF = true;
        }
        return r;
    }

    /* (non-Javadoc)
     * @see java.io.InputStream#read(byte[])
     */
    public int read(byte[] b) throws IOException {
        return read(b, 0, b.length);
    }

    /* (non-Javadoc)
     * @see java.io.InputStream#reset()
     */
    public synchronized void reset() throws IOException {
        in.reset();
        count = mark;
        pastEOF = (count > max);
    }

    /* (non-Javadoc)
     * @see java.io.InputStream#skip(long)
     */
    public long skip(long n) throws IOException {
        if(pastEOF) {
            return 0;
        }
        if(max != 0) {
            n = Math.min(n, max-count);
        }
        long skipped = in.skip(n);
        count += skipped;
        if(count >= max) {
            pastEOF = true;
        }
        return skipped;
    }
}
