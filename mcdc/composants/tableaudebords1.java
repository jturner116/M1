package composants;

public class TableauDeBords {
    public Moteur monMoteur;
    public Reservoir monReservoir;
    public CKPlus monCompteurKilo;
    public CVPlus monCompteurVitesse(monCompteurKilo, monMoteur);
    
   public void toString() {
       System.out.println("Tableau de bord :");
       System.out.println("Kilometrage : " + monCompteurKilo.distanceParcourue());
       System.out.println("Vitesse : " + monCompteurVitesse.vitesseMoyenne());
       System.out.println("Moteur : " + monMoteur.dureeDeFonctionnement());
       System.out.println("Reservoir : " + monReservoir.getVolumeDisponible());
   }

    public void reset() {
        monCompteurKilo.reset();
        monCompteurVitesse.reset();
    }
}