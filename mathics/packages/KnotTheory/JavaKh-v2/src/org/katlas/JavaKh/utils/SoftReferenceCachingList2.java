package org.katlas.JavaKh.utils;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class SoftReferenceCachingList2<Element extends Serializable> extends AbstractList<Element> implements SerializingList<Element> {
	@SuppressWarnings("unused")
	private static final Log log = LogFactory.getLog(SoftReferenceCachingList2.class);

	private final SerializingList<Element> innerList;
	private final NavigableMap<Integer, SoftReference<Element>> softReferences;
	
	public SoftReferenceCachingList2(SerializingList<Element> innerList) {
		this.innerList = innerList;
		this.softReferences = new TreeMap<Integer, SoftReference<Element>>();
	}
	
	@Override
	public synchronized Element get(int index) {
		// try to use a SoftReference
		Reference<Element> reference = softReferences.get(index);
		if(reference != null) {
			Element element = reference.get();
			if(element != null) {
				return element;
			} else {
				log.info("Dropping an expired soft reference.");
				softReferences.remove(index);
			}
		}
		
		// otherwise, retrieve from the innerList, and save a SoftReference
		Element element = innerList.get(index);
		softReferences.put(index, new SoftReference<Element>(element));
		return element;
	}
	
	@Override
	public synchronized int size() {
		return innerList.size();
	}
	
	@Override
	public synchronized boolean add(Element element) {		
		innerList.add(element);
		softReferences.put(size() - 1, new SoftReference<Element>(element));	
		return true;
	}

	@Override
	public synchronized void add(int index, Element element) {
		throw new UnsupportedOperationException();
	}
	
	@Override
	public synchronized void clear() {
		softReferences.clear();
		innerList.clear();
	}

	@Override
	public synchronized Element remove(int index) {
		Element result = innerList.remove(index);
		softReferences.remove(index);
		List<Integer> indexesToDecrement = new ArrayList<Integer>(softReferences.tailMap(index, false).keySet());
		for(int i : indexesToDecrement) {
			softReferences.put(i - 1, softReferences.get(i));
			softReferences.remove(i);
		}
		return result;
	}

	@Override
	public synchronized Element set(int index, Element element) {
		innerList.set(index, element);
		softReferences.put(index, new SoftReference<Element>(element));
		return null;
	}

	public List<File> getSerializedForms() throws IOException {
		return innerList.getSerializedForms();
	}

	public synchronized void setSerializedForm(int index, String hash, InputStream is)
			throws IOException {
		softReferences.remove(index);
		innerList.setSerializedForm(index, hash, is);
	}
	
}
