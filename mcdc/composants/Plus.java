package autrecomposants;

public class Plus {
	private CompteurKilométrique monCompteurKilométrique;
	private int kilométragePartiel;
	private Moteur monMoteur;
	private double duréeDeFntDeRéf;

	public Plus(CompteurKilométrique monCompteurKilométrique,
			    Moteur monMoteur) {
		this.monCompteurKilométrique = monCompteurKilométrique;
		kilométragePartiel = monCompteurKilométrique.getKilométrage();
		this.monMoteur = monMoteur;
		duréeDeFntDeRéf = monMoteur.duréeDeFonctionnement();
	}

	public int distanceParcourue() {
		// retourne le kilométrage parcouru depuis le dernier reset
		return monCompteurKilométrique.getKilométrage() - kilométragePartiel;
	}
	public double vitesseMoyenne() {
		// retourne la vitesse moyenne depuis le dernier reset
		return distanceParcourue() / (monMoteur.duréeDeFonctionnement() - duréeDeFntDeRéf);
	}
	public void reset() {
		// mémorise le kilométrage et la durée d'utilisation courants pour les futurs calculs de moyenne
		kilométragePartiel = monCompteurKilométrique.getKilométrage();
		duréeDeFntDeRéf = monMoteur.duréeDeFonctionnement();
	}
}