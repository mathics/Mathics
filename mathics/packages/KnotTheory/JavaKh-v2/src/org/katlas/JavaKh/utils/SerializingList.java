package org.katlas.JavaKh.utils;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.List;


public interface SerializingList<E extends Serializable> extends List<E> {

	public List<File> getSerializedForms() throws IOException;
	public void setSerializedForm(int index, String hash, InputStream is) throws IOException;
		
}
