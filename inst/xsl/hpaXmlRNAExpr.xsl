<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="xml" omit-xml-declaration="yes" indent="yes"/>
    <xsl:strip-space elements="*"/>

    <xsl:param name="section_type"/>
    <xsl:param name="multi_child"/>

    <xsl:template match="/proteinAtlas">
     <root>
       <xsl:apply-templates select="entry/rnaExpression/data[name(*[1])=$section_type]/*[name()=$multi_child]"/>
     </root>
    </xsl:template>

    <xsl:template match="data/*">
     <data>
        <xsl:apply-templates select="ancestor::rnaExpression/@*"/>
        <rnaSpecificity>
            <xsl:value-of select="ancestor::rnaExpression/rnaSpecificity/@description"/>
        </rnaSpecificity>
	    <xsl:apply-templates select="ancestor::data/*[name()=$section_type]/@*"/>
	    <xsl:element name="tissue"><xsl:value-of select="ancestor::data/*[name()=$section_type]/text()"/></xsl:element>
	    <xsl:apply-templates select="@*"/>
     </data>
    </xsl:template>

    <xsl:template match="rnaExpression/@*|
                         rnaExpression/data/tissue/@*|
                         rnaExpression/data/cellLine/@*|
                         rnaExpression/data/bloodCell/@*|
                         rnaExpression/data/level/@*|
                         rnaExpression/data/RNASample/@*">
      <xsl:element name="{concat(name(..), '_', name())}">
	     <xsl:value-of select="."/>
      </xsl:element>
    </xsl:template>
    
    
</xsl:stylesheet>
