{-# LANGUAGE OverloadedStrings #-}

module FHIR.Validation.Views (
    landingPage,
) where

import Lucid
import Lucid.Base (makeAttributes)

landingPage :: Html ()
landingPage = doctypehtml_ $ do
    head_ $ do
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        title_ "FHIR Validation Service"
        script_ [src_ "https://cdn.tailwindcss.com"] ("" :: String)
        link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com"]
        link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com", makeAttributes "crossorigin" ""]
        link_ [href_ "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap", rel_ "stylesheet"]
        style_ "body { font-family: 'Inter', sans-serif; }"
    body_ [class_ "bg-gradient-to-br from-slate-900 via-slate-800 to-slate-900 min-h-screen"] $ do
        -- Navigation
        nav_ [class_ "border-b border-slate-700/50 backdrop-blur-sm bg-slate-900/50 sticky top-0 z-50"] $ do
            div_ [class_ "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8"] $ do
                div_ [class_ "flex justify-between h-16 items-center"] $ do
                    div_ [class_ "flex items-center space-x-3"] $ do
                        div_ [class_ "w-10 h-10 bg-gradient-to-br from-emerald-400 to-teal-500 rounded-lg flex items-center justify-center"] $ do
                            span_ [class_ "text-white font-bold text-lg"] "F"
                        span_ [class_ "text-white font-semibold text-xl"] "FHIR Validation"
                    div_ [class_ "hidden md:flex items-center space-x-8"] $ do
                        a_ [href_ "#features", class_ "text-slate-300 hover:text-white transition-colors"] "Features"
                        a_ [href_ "#architecture", class_ "text-slate-300 hover:text-white transition-colors"] "Architecture"
                        a_ [href_ "#roadmap", class_ "text-slate-300 hover:text-white transition-colors"] "Roadmap"
                        a_ [href_ "https://github.com/ih1d/fhir", class_ "text-slate-300 hover:text-white transition-colors"] "GitHub"

        -- Hero Section
        section_ [class_ "relative overflow-hidden"] $ do
            div_ [class_ "absolute inset-0 bg-gradient-to-r from-emerald-500/10 to-teal-500/10"] ""
            div_ [class_ "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-24 lg:py-32"] $ do
                div_ [class_ "text-center"] $ do
                    div_ [class_ "inline-flex items-center px-4 py-2 rounded-full bg-emerald-500/10 border border-emerald-500/20 mb-8"] $ do
                        span_ [class_ "text-emerald-400 text-sm font-medium"] "Type-Safe Healthcare Interoperability"
                    h1_ [class_ "text-4xl sm:text-5xl lg:text-6xl font-bold text-white mb-6"] $ do
                        "FHIR Validation"
                        br_ []
                        span_ [class_ "text-transparent bg-clip-text bg-gradient-to-r from-emerald-400 to-teal-400"] "in Haskell"
                    p_ [class_ "text-xl text-slate-400 max-w-3xl mx-auto mb-10"] $ do
                        "A comprehensive, type-safe FHIR R4/R5 validation library built with Haskell. "
                        "Validate resources against StructureDefinitions, FHIRPath expressions, and ValueSet bindings."
                    div_ [class_ "flex flex-col sm:flex-row gap-4 justify-center"] $ do
                        a_ [href_ "#features", class_ "inline-flex items-center justify-center px-8 py-4 rounded-lg bg-gradient-to-r from-emerald-500 to-teal-500 text-white font-semibold hover:from-emerald-600 hover:to-teal-600 transition-all shadow-lg shadow-emerald-500/25"] "Explore Features"
                        a_ [href_ "https://github.com/ih1d/fhir", class_ "inline-flex items-center justify-center px-8 py-4 rounded-lg bg-slate-800 text-white font-semibold hover:bg-slate-700 transition-all border border-slate-700"] "View on GitHub"

        -- Features Section
        section_ [id_ "features", class_ "py-24 bg-slate-900/50"] $ do
            div_ [class_ "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8"] $ do
                div_ [class_ "text-center mb-16"] $ do
                    h2_ [class_ "text-3xl sm:text-4xl font-bold text-white mb-4"] "Core Features"
                    p_ [class_ "text-slate-400 text-lg max-w-2xl mx-auto"] "Built for correctness and performance, leveraging Haskell's type system for robust FHIR validation."
                div_ [class_ "grid md:grid-cols-2 lg:grid-cols-3 gap-8"] $ do
                    featureCard
                        "StructureDefinition Evaluation"
                        "Validate FHIR resources against their StructureDefinitions with full support for slicing, extensions, and constraints."
                        structureIcon
                    featureCard
                        "Cardinality Validation"
                        "Enforce min/max cardinality constraints on all elements, ensuring resources conform to their profiles."
                        cardinalityIcon
                    featureCard
                        "FHIRPath Expressions"
                        "Evaluate FHIRPath expressions for invariant validation, supporting a comprehensive subset of the specification."
                        fhirpathIcon
                    featureCard
                        "ValueSet Binding"
                        "Validate coded elements against ValueSet bindings with support for required, extensible, and preferred strengths."
                        valuesetIcon
                    featureCard
                        "US Core Profiles"
                        "Out-of-the-box support for US Core Implementation Guide profiles for healthcare interoperability in the United States."
                        uscoreIcon
                    featureCard
                        "Type-Safe API"
                        "Leverage Haskell's strong type system to catch validation errors at compile time and ensure correctness."
                        typesafeIcon

        -- Architecture Section
        section_ [id_ "architecture", class_ "py-24"] $ do
            div_ [class_ "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8"] $ do
                div_ [class_ "text-center mb-16"] $ do
                    h2_ [class_ "text-3xl sm:text-4xl font-bold text-white mb-4"] "Workspace Architecture"
                    p_ [class_ "text-slate-400 text-lg max-w-2xl mx-auto"] "A modular multi-package Cabal workspace designed for extensibility and maintainability."
                div_ [class_ "grid lg:grid-cols-2 gap-12 items-center"] $ do
                    div_ [class_ "space-y-6"] $ do
                        packageCard "fhir-types" "Core FHIR R4 data types with full type coverage" "Phase 1" True
                        packageCard "fhir-json" "Aeson-based JSON serialization with FHIR-specific patterns" "Phase 2" False
                        packageCard "fhir-xml" "xml-conduit XML serialization support" "Phase 2" False
                        packageCard "fhir-validation" "Validation against StructureDefinitions" "Phase 3" True
                        packageCard "fhir-client" "Type-safe FHIR server client" "Phase 2" False
                    div_ [class_ "bg-slate-800/50 rounded-2xl p-8 border border-slate-700/50"] $ do
                        h3_ [class_ "text-xl font-semibold text-white mb-4"] "Dependency Hierarchy"
                        div_ [class_ "font-mono text-sm text-slate-300 space-y-2"] $ do
                            div_ [class_ "flex items-center space-x-2"] $ do
                                span_ [class_ "text-emerald-400"] "fhir-types"
                            div_ [class_ "flex items-center space-x-2 ml-4"] $ do
                                span_ [class_ "text-slate-500"] "|"
                            div_ [class_ "flex items-center space-x-2 ml-4"] $ do
                                span_ [class_ "text-slate-500"] "+->"
                                span_ [class_ "text-teal-400"] "fhir-json"
                                span_ [class_ "text-slate-500"] "/"
                                span_ [class_ "text-teal-400"] "fhir-xml"
                            div_ [class_ "flex items-center space-x-2 ml-8"] $ do
                                span_ [class_ "text-slate-500"] "|"
                            div_ [class_ "flex items-center space-x-2 ml-8"] $ do
                                span_ [class_ "text-slate-500"] "+->"
                                span_ [class_ "text-amber-400"] "fhir-validation"
                            div_ [class_ "flex items-center space-x-2 ml-12"] $ do
                                span_ [class_ "text-slate-500"] "|"
                            div_ [class_ "flex items-center space-x-2 ml-12"] $ do
                                span_ [class_ "text-slate-500"] "+->"
                                span_ [class_ "text-purple-400"] "fhir-client"
                        div_ [class_ "mt-6 pt-6 border-t border-slate-700"] $ do
                            h4_ [class_ "text-sm font-medium text-slate-400 mb-3"] "Technical Stack"
                            div_ [class_ "flex flex-wrap gap-2"] $ do
                                techBadge "GHC 9.6+"
                                techBadge "Aeson"
                                techBadge "xml-conduit"
                                techBadge "Hspec"
                                techBadge "QuickCheck"

        -- Roadmap Section
        section_ [id_ "roadmap", class_ "py-24 bg-slate-900/50"] $ do
            div_ [class_ "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8"] $ do
                div_ [class_ "text-center mb-16"] $ do
                    h2_ [class_ "text-3xl sm:text-4xl font-bold text-white mb-4"] "Development Roadmap"
                    p_ [class_ "text-slate-400 text-lg max-w-2xl mx-auto"] "Our phased approach to building a complete FHIR implementation in Haskell."
                div_ [class_ "max-w-3xl mx-auto"] $ do
                    roadmapPhase "Phase 1" "Core Types" "Complete" "Building foundational FHIR R4 data types with full type safety" True
                    roadmapPhase "Phase 2" "Serialization" "In Progress" "JSON and XML serialization with FHIR-specific patterns" False
                    roadmapPhase "Phase 3" "Validation" "Planned" "StructureDefinition evaluation and FHIRPath support" False
                    roadmapPhase "Phase 4" "Client" "Planned" "Type-safe FHIR server client implementation" False

        -- Footer
        footer_ [class_ "border-t border-slate-700/50 py-12"] $ do
            div_ [class_ "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8"] $ do
                div_ [class_ "flex flex-col md:flex-row justify-between items-center"] $ do
                    div_ [class_ "flex items-center space-x-3 mb-4 md:mb-0"] $ do
                        div_ [class_ "w-8 h-8 bg-gradient-to-br from-emerald-400 to-teal-500 rounded-lg flex items-center justify-center"] $ do
                            span_ [class_ "text-white font-bold text-sm"] "F"
                        span_ [class_ "text-slate-400"] "FHIR Validation"
                    div_ [class_ "flex items-center space-x-6"] $ do
                        a_ [href_ "https://github.com/ih1d/fhir", class_ "text-slate-400 hover:text-white transition-colors"] "GitHub"
                        span_ [class_ "text-slate-600"] "|"
                        span_ [class_ "text-slate-500 text-sm"] "Built with Haskell, Servant & Lucid2"

-- Helper Components
featureCard :: String -> String -> Html () -> Html ()
featureCard title description icon =
    div_ [class_ "bg-slate-800/50 rounded-xl p-6 border border-slate-700/50 hover:border-emerald-500/30 transition-all group"] $ do
        div_ [class_ "w-12 h-12 bg-gradient-to-br from-emerald-500/20 to-teal-500/20 rounded-lg flex items-center justify-center mb-4 group-hover:from-emerald-500/30 group-hover:to-teal-500/30 transition-all"] $ do
            icon
        h3_ [class_ "text-lg font-semibold text-white mb-2"] (toHtml title)
        p_ [class_ "text-slate-400 text-sm"] (toHtml description)

packageCard :: String -> String -> String -> Bool -> Html ()
packageCard name description phase active =
    div_ [class_ $ "flex items-center p-4 rounded-lg border " <> if active then "bg-emerald-500/10 border-emerald-500/30" else "bg-slate-800/30 border-slate-700/50"] $ do
        div_ [class_ $ "w-3 h-3 rounded-full mr-4 " <> if active then "bg-emerald-400" else "bg-slate-600"] ""
        div_ [class_ "flex-1"] $ do
            div_ [class_ "flex items-center justify-between mb-1"] $ do
                span_ [class_ "font-mono text-white font-medium"] (toHtml name)
                span_ [class_ $ "text-xs px-2 py-1 rounded " <> if active then "bg-emerald-500/20 text-emerald-400" else "bg-slate-700 text-slate-400"] (toHtml phase)
            p_ [class_ "text-slate-400 text-sm"] (toHtml description)

roadmapPhase :: String -> String -> String -> String -> Bool -> Html ()
roadmapPhase phase title status description isFirst =
    div_ [class_ "relative pl-8 pb-8 last:pb-0"] $ do
        -- Vertical line
        if isFirst
            then div_ [class_ "absolute left-3 top-3 bottom-0 w-px bg-gradient-to-b from-emerald-500 to-slate-700"] ""
            else div_ [class_ "absolute left-3 top-0 bottom-0 w-px bg-slate-700"] ""
        -- Dot
        div_
            [ class_ $
                "absolute left-0 top-0 w-6 h-6 rounded-full border-2 flex items-center justify-center "
                    <> case status of
                        "Complete" -> "bg-emerald-500 border-emerald-500"
                        "In Progress" -> "bg-slate-900 border-amber-500"
                        _ -> "bg-slate-900 border-slate-600"
            ]
            $ do
                case status of
                    "Complete" -> span_ [class_ "text-white text-xs"] "v"
                    "In Progress" -> span_ [class_ "w-2 h-2 bg-amber-500 rounded-full animate-pulse"] ""
                    _ -> span_ [] ""
        div_ [class_ "ml-4"] $ do
            div_ [class_ "flex items-center space-x-3 mb-2"] $ do
                span_ [class_ "text-sm font-medium text-slate-500"] (toHtml phase)
                span_ [class_ "text-xl font-semibold text-white"] (toHtml title)
                span_
                    [ class_ $
                        "text-xs px-2 py-1 rounded "
                            <> case status of
                                "Complete" -> "bg-emerald-500/20 text-emerald-400"
                                "In Progress" -> "bg-amber-500/20 text-amber-400"
                                _ -> "bg-slate-700 text-slate-400"
                    ]
                    (toHtml status)
            p_ [class_ "text-slate-400"] (toHtml description)

techBadge :: String -> Html ()
techBadge name = span_ [class_ "px-3 py-1 bg-slate-700/50 rounded-full text-xs text-slate-300"] (toHtml name)

-- SVG Icons
structureIcon :: Html ()
structureIcon = toHtmlRaw ("<svg class=\"w-6 h-6 text-emerald-400\" fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\"><path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M19 11H5m14 0a2 2 0 012 2v6a2 2 0 01-2 2H5a2 2 0 01-2-2v-6a2 2 0 012-2m14 0V9a2 2 0 00-2-2M5 11V9a2 2 0 012-2m0 0V5a2 2 0 012-2h6a2 2 0 012 2v2M7 7h10\"></path></svg>" :: String)

cardinalityIcon :: Html ()
cardinalityIcon = toHtmlRaw ("<svg class=\"w-6 h-6 text-emerald-400\" fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\"><path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M7 20l4-16m2 16l4-16M6 9h14M4 15h14\"></path></svg>" :: String)

fhirpathIcon :: Html ()
fhirpathIcon = toHtmlRaw ("<svg class=\"w-6 h-6 text-emerald-400\" fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\"><path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M8 9l3 3-3 3m5 0h3M5 20h14a2 2 0 002-2V6a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z\"></path></svg>" :: String)

valuesetIcon :: Html ()
valuesetIcon = toHtmlRaw ("<svg class=\"w-6 h-6 text-emerald-400\" fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\"><path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2m-6 9l2 2 4-4\"></path></svg>" :: String)

uscoreIcon :: Html ()
uscoreIcon = toHtmlRaw ("<svg class=\"w-6 h-6 text-emerald-400\" fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\"><path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M3 21v-4m0 0V5a2 2 0 012-2h6.5l1 1H21l-3 6 3 6h-8.5l-1-1H5a2 2 0 00-2 2zm9-13.5V9\"></path></svg>" :: String)

typesafeIcon :: Html ()
typesafeIcon = toHtmlRaw ("<svg class=\"w-6 h-6 text-emerald-400\" fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\"><path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M9 12l2 2 4-4m5.618-4.016A11.955 11.955 0 0112 2.944a11.955 11.955 0 01-8.618 3.04A12.02 12.02 0 003 9c0 5.591 3.824 10.29 9 11.622 5.176-1.332 9-6.03 9-11.622 0-1.042-.133-2.052-.382-3.016z\"></path></svg>" :: String)
