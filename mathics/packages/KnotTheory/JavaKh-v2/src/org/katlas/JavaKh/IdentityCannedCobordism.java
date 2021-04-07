package org.katlas.JavaKh;

import org.katlas.JavaKh.interfaces.CannedCobordism;

public class IdentityCannedCobordism implements CannedCobordism {

	private final Cap cap;
	
	public IdentityCannedCobordism(Cap cap) {
		this.cap = cap;
	}
	
	public CannedCobordism compose(int start, CannedCobordism cc, int cstart,
			int nc) {
		// lazy...
		return cc.compose(cstart, this, start, nc);
	}

	public boolean isIsomorphism() {
		return true;
	}

	public void reverseMaps() {
		// do nothing!
	}

	public CannedCobordism compose(CannedCobordism m) {
		return m;
	}

	public Cap source() {
		return cap;
	}

	public Cap target() {
		return cap;
	}

}
