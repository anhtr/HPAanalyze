<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="xml" omit-xml-declaration="yes" indent="yes"/>
    <xsl:strip-space elements="*"/>

    <xsl:template match="/proteinAtlas">
     <data>
       <xsl:apply-templates select="entry/antibody/tissueExpression/data/patient"/>
     </data>
    </xsl:template>

    <xsl:template match="patient">
     <xsl:copy>
       <xsl:apply-templates select="patientId|age|sex|level|quantity|location|sample[1]/assayImage/image/imageUrl"/>
       <xsl:apply-templates select="sample[1]/snomedParameters"/>
     </xsl:copy>
    </xsl:template>

     <xsl:template match="patientId|age|sex|quantity|location|sample[1]/assayImage/image/imageUrl">
         <xsl:element name="{local-name()}">
           <xsl:apply-templates select="node()|@*"/>
         </xsl:element>
     </xsl:template>

     <xsl:template match="level[@type='staining' or @type='intensity']">
        <xsl:element name="{@type}"> 
            <xsl:value-of select="."/>
       </xsl:element>
     </xsl:template>

    <xsl:template match="sample[1]/snomedParameters">
        <xsl:apply-templates select="snomed" mode="sc"/>
        <xsl:apply-templates select="snomed" mode="td"/>
    </xsl:template>

    <xsl:template match="snomed" mode="sc">
       <xsl:element name="{concat('snomedCode', position())}">
            <xsl:value-of select="@snomedCode"/>
       </xsl:element>
    </xsl:template>

    <xsl:template match="snomed" mode="td">
       <xsl:element name="{concat('tissueDescription', position())}">
            <xsl:value-of select="@tissueDescription"/>
       </xsl:element>
    </xsl:template>

</xsl:stylesheet>

