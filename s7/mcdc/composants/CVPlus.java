package composants;

public class CVPlus {
	private CKPlus monCKPlus;
	private Moteur monMoteur;
	private double dureeDeFntDeRef;

	public CVPlus(CKPlus monCKPlus, Moteur monMoteur) {
		this.monCKPlus = monCKPlus;
		this.monMoteur = monMoteur;
		dureeDeFntDeRef = monMoteur.dureeDeFonctionnement();
	}

	public double vitesseMoyenne() {
		// retourne la vitesse moyenne depuis le dernier reset
		return monCKPlus.distanceParcourue() / (monMoteur.dureeDeFonctionnement() - dureeDeFntDeRef);
	}

	public void reset() {
		// m�morise la dur�e d'utilisation courante pour les futurs calculs de moyenne
		dureeDeFntDeRef = monMoteur.dureeDeFonctionnement();
	}
}
