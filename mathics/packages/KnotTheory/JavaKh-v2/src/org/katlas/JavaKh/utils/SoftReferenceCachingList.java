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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class SoftReferenceCachingList<Element extends Serializable> extends AbstractList<Element> implements SerializingList<Element> {
	@SuppressWarnings("unused")
	private static final Log log = LogFactory.getLog(SoftReferenceCachingList.class);

	private final SerializingList<Element> innerList;
	private final List<SoftReference<Element>> softReferenceList;
	
	public SoftReferenceCachingList(SerializingList<Element> innerList) {
		this.innerList = innerList;
		this.softReferenceList = new ArrayList<SoftReference<Element>>(innerList.size());
		for(@SuppressWarnings("unused") Element e : innerList) {
			this.softReferenceList.add(null);
		}
	}
	
	@Override
	public synchronized Element get(int index) {
		// try to use a SoftReference
		Reference<Element> reference = softReferenceList.get(index);
		if(reference != null) {
			Element element = reference.get();
			if(element != null) {
				return element;
			}
		}
		
		// otherwise, retrieve from the innerList, and save a SoftReference
		Element element = innerList.get(index);
		softReferenceList.set(index, new SoftReference<Element>(element));
		return element;
	}
	
	@Override
	public synchronized int size() {
		return innerList.size();
	}
	
	@Override
	public synchronized boolean add(Element element) {		
		innerList.add(element);
		softReferenceList.add(new SoftReference<Element>(element));	
		return true;
	}

	@Override
	public synchronized void add(int index, Element element) {
		innerList.add(index, element);
		softReferenceList.add(index, new SoftReference<Element>(element));
	}
	
	@Override
	public synchronized void clear() {
		softReferenceList.clear();
		innerList.clear();
	}

	@Override
	public synchronized Element remove(int index) {
		softReferenceList.remove(index);
		innerList.remove(index);		
		return null;
	}

	@Override
	public synchronized Element set(int index, Element element) {
		innerList.set(index, element);
		softReferenceList.set(index, new SoftReference<Element>(element));
		return null;
	}

	public List<File> getSerializedForms() throws IOException {
		return innerList.getSerializedForms();
	}

	public synchronized void setSerializedForm(int index, String hash, InputStream is)
			throws IOException {
		softReferenceList.set(index, null);
		innerList.setSerializedForm(index, hash, is);
	}
	
}
