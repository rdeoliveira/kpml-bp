//import java.io.IOException;
//import java.util.LinkedList;
//import java.util.List;
//import hals.Hals;
//import hals.ds.HalsResource;
//import hals.engine.HalsEngine;
//import hals.engine.HalsTraverser;
//import hals.engine.Inquirer;
//import hals.engine.InquirerInPrompt;
//import hals.engine.UserConsideration;
//import hals.engine.UserConsiderationInPrompt;
//import hal.meaning.Meaning;
//import hal.meaning.Meaning;
//import hal.s.resource.ResourceParser;
//import hal.s.resource.ResourcePart;
//import hal.s.resource.ResourceTokenizer;

import java.util.Map;
import de.unibremen.linguistics.Base;
import de.unibremen.linguistics.ItemMapSetter;
import de.unibremen.linguistics.TextSynthesizer;
import de.unibremen.phylosophy.Ontology;
import de.unibremen.phylosophy.Reasoner;
import hal.Indexable;
import hal.meaning.Idea;
import hal.meaning.Name;
import hal.s.writing.Item;
import hal.s.writing.Klass;

public class RunExamples implements Base{
	
	static Integer index = 1;
	
	public static Map <String, Indexable> itemMap;
	private static Reasoner reasoner;
	
	public Name accessConfigurationStatus(Idea elementIdea){
		//return accessStatus(elementIdea, "Configuration", "configuration");
		//return accessStatus(elementIdea, "SpatialLocating", "configuration");
		if (elementIdea==null){
			return null;
		}
		//System.out.println(elementIdea.getType());
		if (elementIdea.getType().equals("Relating") 
				|| elementIdea.getType().equals("Existence")
					|| elementIdea.getType().equals("Perception")
						|| elementIdea.getType().equals("AffectingAction")
								|| elementIdea.getType().equals("AffectingSpatialAction")
									|| elementIdea.getType().equals("NonAffectingAction")
										|| elementIdea.getType().equals("NonAffectingSpatialDoing")
											|| elementIdea.getType().equals("NonAffectingOrientationChange")
												|| elementIdea.getType().equals("SpatialLocating")){
			return new Name("configuration");
		}
		return new Name ("not-configuration");
	}
	
	public Name accessCircumstanceStatus(Idea elementIdea){
		if (elementIdea.getType().equals("GeneralizedRoute")
				|| elementIdea.getType().equals("GeneralizedLocation")){
			return new Name("circumstance");
		}
		return null;
	}
	
	private Name accessStatus(Idea elementIdea, String type, String nameIdex) {
		if (elementIdea==null){
			return null;
		}
		//System.out.println(type + " # " + elementIdea.getType() + " # " + reasoner.checkSubclass(type, elementIdea.getType()));
		if (reasoner.checkSubclass(type, elementIdea.getType())){
			return new Name(nameIdex);
		}
		return null;
	}
	
	public Name accessIsThereCircumstance(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		Idea circumstance = elementIdea.getIdea("placement");
		//System.out.println("placement: " + circumstance);
		if (circumstance == null){
			return null;
		}
		return new Name("yes");
	}
	
	public Name accessIsThereRelatum(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		Idea circumstance = elementIdea.getIdea("relatum");
		//System.out.println("placement: " + circumstance);
		if (circumstance == null){
			return null;
		}
		return new Name("yes");
	}
	
	public Name accessIsSourceMentioned(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		Idea circumstance = elementIdea.getIdea("source");
		if (circumstance == null){
			return null;
		}
		return new Name("yes");
	}
	
	public Name accessIsDestinationMentioned(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		Idea circumstance = elementIdea.getIdea("destination");
		if (circumstance == null){
			return null;
		}
		return new Name("yes");
	}
	
	public Name accessIsPathPlacementMentioned(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		Idea circumstance = elementIdea.getIdea("path-placement");
		if (circumstance == null){
			return null;
		}
		return new Name("yes");
	}
	
	public Name accessComplexity(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		Idea nextIdea = elementIdea.getIdea("next");
		//System.out.println("placement: " + circumstance);
		if (nextIdea == null){
			return null;
		}
		return new Name("complex");
	}
	
	public Idea accessCircumstanceIdea(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getIdea("placement");
	}
	
	public Name accessLocationStatus(Idea elementIdea){
		return accessStatus(elementIdea, "GeneralizedLocation", "location");
	}
	
	public Name accessRouteStatus(Idea elementIdea){
		return accessStatus(elementIdea, "GeneralizedRoute", "route");
	}
	
	public Name accessPlacementType(Idea elementIdea){
		//System.out.println(elementIdea);
		if (elementIdea==null){
			return null;
		}
		if (elementIdea.getType().equals("GeneralizedRoute")){
			return new Name("route");
		}
		return new Name("location");
	}
	
	public Name accessReferenceType(Idea elementIdea){
		//System.out.println(elementIdea);
		if (elementIdea==null){
			return null;
		}
		Name name = elementIdea.getName("name");
		if (name == null){
			return null; 
		}
		return new Name("thing");
	}
	
	public Name accessRouteType(Idea elementIdea){
		if (elementIdea.hasIdea("path-placement")){
			return new Name("path-placement");
		}
		return null;
	}
	
	public Name accessConfigurationType(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		//System.out.println(elementIdea.getType());
		if (elementIdea.getType().equals("Relating")){
			return new Name("relating");
		}
		if (elementIdea.getType().equals("Existence")){
			return new Name("existence");
		}
		if (elementIdea.getType().equals("Perception")){
			return new Name("perception");
		}
		if (elementIdea.getType().equals("AffectingAction")){
			return new Name("affecting-action");
		}
		if (elementIdea.getType().equals("AffectingSpatialAction")){
			return new Name("affecting-spatial-action");
		}
		if (elementIdea.getType().equals("NonAffectingAction")){
			return new Name("non-affecting-action");
		}
		if (elementIdea.getType().equals("NonAffectingSpatialDoing")){
			return new Name("non-affecting-spatial-doing");
		}
		if (elementIdea.getType().equals("NonAffectingOrientationChange")){
			return new Name("non-affecting-orientation-change");
		}
		return new Name ("spatial-locating");
	}
	
	public Name accessClassName(Idea elementIdea){
		//System.out.println(elementIdea);
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getName("class-name");
	}
	
	public Name accessNameIdentifiability(Idea elementIdea){
		//System.out.println(elementIdea);
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getName("identifiability");
	}
	
	public Name accessName(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getName("name");
	}
	
	public Idea accessLocatumIdea(Idea processIdea){
		if (processIdea==null){
			return null;
		}
		return processIdea.getIdea("locatum");
	}
	
	public Idea accessPlacementIdea(Idea processIdea){
		if (processIdea==null){
			return null;
		}
		return processIdea.getIdea("placement");
	}
	
	public Idea accessRouteIdea(Idea processIdea){
		if (processIdea==null){
			return null;
		}
		return processIdea.getIdea("route");
	}
	
	public Idea accessDirectionIdea(Idea processIdea){
		if (processIdea==null){
			return null;
		}
		return processIdea.getIdea("direction");
	}
	
	public Idea accessSourceIdea(Idea processIdea){
		if (processIdea==null){
			return null;
		}
		return processIdea.getIdea("source");
	}
	
	public Idea accessDestinationIdea(Idea processIdea){
		if (processIdea==null){
			return null;
		}
		return processIdea.getIdea("destination");
	}
	
	public Idea accessPathPlacementIdea(Idea processIdea){
		if (processIdea==null){
			return null;
		}
		return processIdea.getIdea("path-placement");
	}
	
	public Idea accessAttribuendIdea(Idea processIdea){
		if (processIdea==null){
			return null;
		}
		//System.out.println(processIdea.getIdea("attribuend"));
		return processIdea.getIdea("attribuend");
	}
	
	public Idea accessNextIdea(Idea thingIdea){
		if (thingIdea==null){
			return null;
		}
		//System.out.println(processIdea.getIdea("attribuend"));
		return thingIdea.getIdea("next");
	}
	
	public Idea accessAttributeIdea(Idea processIdea){
		if (processIdea==null){
			return null;
		}
		//System.out.println(processIdea.getIdea("attribute"));
		return processIdea.getIdea("attribute");
	}
	
	public Idea accessExistentIdea(Idea processIdea){
		if (processIdea==null){
			return null;
		}
		//System.out.println(processIdea.getIdea("attribute"));
		return processIdea.getIdea("existent");
	}
	
	public Idea accessRelatumIdea(Idea locationIdea){
		if (locationIdea==null){
			return null;
		}
		return locationIdea.getIdea("relatum");
	}
	
	public Idea accessSenserIdea(Idea processIdea){
		if (processIdea==null){
			return null;
		}
		return processIdea.getIdea("senser");
	}
	
	public Idea accessPhenomenonIdea(Idea processIdea){
		if (processIdea==null){
			return null;
		}
		return processIdea.getIdea("phenomenon");
	}
	
	public Idea accessSpatialModalityIdea(Idea locationIdea){
		if (locationIdea==null){
			return null;
		}
		return locationIdea.getIdea("hasSpatialModality");
	}
	
	public Idea accessActorIdea(Idea processIdea){
		if (processIdea==null){
			return null;
		}
		return processIdea.getIdea("actor");
	}
	
	public Idea accessActeeIdea(Idea processIdea){
		if (processIdea==null){
			return null;
		}
		return processIdea.getIdea("actee");
	}
	
	public Name accessAPEPPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "AboveProjectionExternal");
	}
	
	public Name accessAPIPPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "AboveProjectionInternal");
	}
	
	public Name accessBPEPPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "BackProjectionExternal");
	}
	
	public Name accessBPIPPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "BackProjectionInternal");
	}
	
	public Name accessCentralPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "Central");
	}
	
	public Name accessConnectionPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "Connection");
	}
	
	public Name accessConnectionType(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getName("subtype");
	}
	
	public Name accessGDNearingType(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		//System.out.println(">>>>" + elementIdea.toString() + " # " + elementIdea.getName("subtype"));
		return elementIdea.getName("subtype");
	}
	
	public Name accessContainmentPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "Containment");
	}
	
	public Name accessContainmentType(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getName("subtype");
	}
	
	public Name accessDOFCPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "DenialOfFunctionControl");
	}
	
	public Name accessDistalPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "Distal");
	}
	
	public Name accessDOFCType(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getName("subtype");
	}
	
	public Name accessDistalType(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getName("subtype");
	}
	
	public Name accessDistributionPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "Distribution");
	}
	
	public Name accessFPEPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "FrontProjectionExternal");
	}
	
	public Name accessGDNearingPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "GeneralDirectionalNearing");
	}
	
	public Name accessGDDistancingPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "GeneralDirectionalDistancing");
	}
	
	public Name accessLateralProjectionPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "LateralProjection");
	}
	
	public Name accessNorthInternalPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "NorthInternal");
	}
	
	public Name accessMultiDirectionalPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "MultiDirectional");
	}
	
	public Name accessPeripheralPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "Peripheral");
	}
	
	public Name accessParthoodPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "Parthood");
	}
	
	public Name accessParthoodType(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getName("subtype");
	}
	
	public Name accessPathRepresentingType(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getName("subtype");
	}
	
	public Name accessPathRepresentingPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "PathRepresenting");
	}
	
	public Name accessProximalPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "Proximal");
	}
	
	public Name accessProximalType(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getName("subtype");
	}
	
	public Name accessQuantitativeDistancePhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "QuantitativeDistance");
	}
	
	public Name accessRNPAPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "RelativeNonProjectionAxial");
	}
	
	public Name accessRightProjectionPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "RightProjection");
	}
	
	public Name accessQuantitativeDistanceType(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getName("subtype");
	}
	
	public Name accessSupportPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "Support");
	}
	
	public Name accessSupportType(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getName("subtype");
	}
	
	public Name accessSurroundingPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "Surrounding");
	}
	
	public Name accessTopographicalPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "Topographical");
	}
	
	public Name accessUPEPhrase(Idea elementIdea){
		return generalAccessModality(elementIdea, "UnderProjectionExternal");
	}
		
	public Name generalAccessModality(Idea elementIdea, String modality){
		if (elementIdea==null){
			return null;
		}
		Idea spatialModality = elementIdea.getIdea("hasSpatialModality");
		if (spatialModality == null){
			return null;
		}
		if (spatialModality.getType().equals(modality)){
			return new Name("yes");
		}
		return new Name("no");
	}
	
	public Name accessQuantitativeDE(Idea elementIdea){
		return generalAccessModalityModification(elementIdea, "QuantitativeDistanceExtent");
	}
		
	public Name accessAccessibility(Idea elementIdea){
		return generalAccessModalityModification(elementIdea, "Accessibility");
	}
	
	public Name accessQualitativeDE(Idea elementIdea){
		return generalAccessModalityModification(elementIdea, "QualitativeDistanceExtent");
	}
	
	public Name accessReciprocal(Idea elementIdea){
		return generalAccessModalityModification(elementIdea, "Reciprocal");
	}
	
	public Name accessSpatialPerspective(Idea elementIdea){
		return generalAccessModalityModification(elementIdea, "SpatialPerspective");
	}
	
	public Name generalAccessModalityModification(Idea elementIdea, String modality){
		if (elementIdea==null){
			return null;
		}
		Idea spatialModality = elementIdea.getIdea("hasSpatialModalityModification");
		if (spatialModality == null){
			return null;
		}
		if (spatialModality.getType().equals(modality)){
			return new Name("yes");
		}
		return new Name("no");
	}
	
	public Name accessNameGender(Name className){
		if (className==null){
			return null;
		}
		Item item = (Item)itemMap.get(className.getIndex());
		if (item==null){
			return null;
		}
		if (item.contains(new Klass("masculine"))){
			return new Name("masculine");
		}
		if (item.contains(new Klass("feminine"))){
			return new Name("feminine");
		}
		return null;
	}
	
	public Idea accessModificationIdea(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getIdea("hasSpatialModalityModification");
	}
	
	public Name accessQuantity(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getName("quantity");
	}
	
	public Name accessUnit(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getName("unit");
	}
	
	public Name accessAccessibilityLevel(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getName("level");
	}
	
	public Name accessQualitativeDEName(Idea elementIdea){
		if (elementIdea==null){
			return null;
		}
		return elementIdea.getName("class-name");
	}
	
//	public static void main(String[] args) throws IOException {
//		Ontology ontology = new Ontology();
//		reasoner = new Reasoner();
//		reasoner.setOntology(ontology);
//		 List<ResourcePart> resourcePartList = new LinkedList<ResourcePart>();
//		 resourcePartList.add(Hals.getResourcePart("synthesis", "", "Writing.hals"));
//		 resourcePartList.add(Hals.getResourcePart("synthesis", "", "Wording.hals"));
//		 resourcePartList.add(Hals.getResourcePart("synthesis", "", "Meaning.hals"));
//		 resourcePartList.add(Hals.getResourcePart("synthesis", "", "Examples.hals"));
//		 ResourceTokenizer tokenizer = new ResourceTokenizer(resourcePartList);
//		 @SuppressWarnings({ "rawtypes", "unchecked" })
//		Inquirer inquirer = new InquirerInPrompt(new RunExamples(), RunExamples.class, false);
//		 ResourceParser parser = new ResourceParser(tokenizer, inquirer);
//		 HalsResource resource = parser.createResource();
//		 itemMap = resource.getItemMap();
//		 HalsEngine engine = new HalsEngine(resource, 
//				 new HalsTraverser(resource, new UserConsiderationInPrompt()));
//		 //Hals.interpretCode(resourcePartList, new RunExamples(), RunExamples.class);
//		 for (Meaning meaning : resource.getMeaningList()){
//			 //System.out.println();
//			 //System.out.print(meaning);
//			 Idea mainIdea = meaning.getIdea("Main");
//			 if (mainIdea == null){
//				 System.err.print("Main idea not found");
//			 } else {
//				 printText(mainIdea, engine);
//				 index++;
//			 }
//		 }
//	}
	
	@SuppressWarnings("static-access")
	@ItemMapSetter
	public void setItemMap(Map<String, Indexable> itemMap) {
		this.itemMap = itemMap;
	}
	
	public static void main(String[] args) {
		Ontology ontology = new Ontology("space");
		reasoner = new Reasoner();
		reasoner.setOntology(ontology);
		TextSynthesizer synthesizer;
		synthesizer = new TextSynthesizer();
		synthesizer.setBase(new RunExamples());
		synthesizer.printSpecifiedTextsInSlots();
		//System.out.println(synthesizer.getResource().toKpmlCode());
	}
	
//	private static void printText(Idea mainIdea, HalsEngine engine) {
//		try {
//			List<String> formList = engine.produce(mainIdea);
//			for (String form : formList){
//				System.out.print(index);
//				System.out.print(": ");
//				System.out.println(form);
//			}
//		} catch(Exception e){
//			e.printStackTrace();
//		}
//	}
}
